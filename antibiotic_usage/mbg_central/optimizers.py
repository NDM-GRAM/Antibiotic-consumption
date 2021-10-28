"""
Created on Wed Dec 13 15:38:00 2017

@author: jco24
"""
# import system-level functions (interact R and python, set library path)
import sys
# load optimizers; test PYTHONPATH is working correctly
try:
    import skopt  # pip install scikit-optimize
except ImportError:
    import warnings
    warnings.warn("PYTHONPATH not configured correctly - applying fix")
    sys.path.append('/ihme/code/geospatial/stacking_optimizers/miniconda2/envs/optimizer_centos/lib/python2.7/site-packages')  # noqa
    import skopt
# standard imports
from scipy import mean
from pandas import read_csv, DataFrame
# load learners
from sklearn.ensemble import (GradientBoostingRegressor,
                              GradientBoostingClassifier)
from xgboost.sklearn import (XGBRegressor,
                             XGBClassifier)
# load scoring_methods
from sklearn.model_selection import cross_val_score


# STANDARDIZING SPACE FUNCTION:
#    convenience function for taking limits from .csv to python list
def space_maker(df):
    ls = []
    for i in range(0, len(df.columns)):
        ls.append(tuple(df.iloc[:, i].dropna()))
    return ls


# HYPERPARAMETER OPTIMIZATION FUNCTION
#      note: skopt.gp_minimize() will return a NaN error if the space boundaries defined in range_file are identical
def optimize(X,
             y,
             range_file,
             learner_choice='brtR',
             optimizer='gp',
             cv_folds=10,
             n_calls=50):
    # read in ranges from range_file
    ranges = read_csv(range_file)
    # get number of features of input data
    n_features = X.shape[1]
    # make dataframe of limits parse-able by objective
    space = space_maker(ranges)
    # make compatible search spaces and objective functions for each learner_choice
    if (learner_choice == 'brtR' or learner_choice == 'brtC'):
        space.append((1, n_features))

        # define objective function to minimize <- skopt
        def objective(params):
            (learning_rate,
             max_depth,
             n_estimators,
             max_features) = params
            if (learner_choice == 'brtR'):
                learner = GradientBoostingRegressor()
            elif (learner_choice == 'brtC'):
                learner = GradientBoostingClassifier()
            learner.set_params(learning_rate=learning_rate,
                               loss='ls',
                               max_depth=max_depth,
                               max_features=max_features,
                               n_estimators=n_estimators,
                               subsample=0.75)
            return -mean(cross_val_score(learner, X, y,
                                         cv=cv_folds, n_jobs=-1,
                                         scoring="neg_mean_absolute_error"))
    elif (learner_choice == 'xgbR' or learner_choice == 'xgbC'):

        # define objective function to minimize <- xgboost
        def objective(params):
            (learning_rate,
             max_depth,
             n_estimators) = params
            if (learner_choice == 'xgbR'):
                learner = XGBRegressor()
            elif (learner_choice == 'xgbC'):
                learner = XGBClassifier()
            learner.set_params(booster='gbtree',
                               learning_rate=learning_rate,
                               max_depth=max_depth,
                               n_estimators=n_estimators,
                               subsample=0.75)
            return -mean(cross_val_score(learner, X, y,
                                         cv=cv_folds, n_jobs=-1,
                                         scoring="neg_mean_absolute_error"))
    else:
        error_lrn = 'not an available choice, use: brtR, brtC, xgbR, or xgbC'
    # run fits: gaussian process, random forest, gradient-boosted regression trees (resp.)
    if (optimizer == 'gp'):
        best_pars = skopt.gp_minimize(objective, space, n_calls=n_calls)
    elif (optimizer == 'rf'):
        best_pars = skopt.forest_minimize(objective, space, n_calls=n_calls)
    elif (optimizer == 'brt'):
        best_pars = skopt.gbrt_minimize(objective, space, n_calls=n_calls)
    else:
        error_opt = 'not an available choice, use: gp, rf, or brt'
    # return fit params conditional on no user type-o's
    if 'error_opt' in locals():
        print(error_opt)
        return None
    elif 'error_lrn' in locals():
        print(error_lrn)
        return None
    else:
        return best_pars.x


def main(bounds_file, out_dir, data_file, optimizer, learner, cv_folds,
         n_calls, jobnum, col_start):
    # LOAD SEARCH SPACE LIMITS
    data = read_csv(data_file)
    data = data.dropna(axis=0, how='any')
    X_data = data.loc[:, data.columns[col_start - 1]:data.columns[len(data.columns) - 1]]
    y_data = data.loc[:, data.columns[0]]

    # RUN HYPERPARAMETER TUNING
    par_fit = optimize(X_data,
                       y_data,
                       range_file=bounds_file,
                       learner_choice=learner,
                       optimizer=optimizer,
                       cv_folds=cv_folds,
                       n_calls=n_calls)

    # SAVE TUNING RESULTS TO FILE
    if (learner == 'brtR' or learner == 'brtC'):
        par_fit_dict = {'shrinkage': par_fit[0],
                        'interaction.depth': par_fit[1],
                        'n.trees': par_fit[2],
                        'n.minobsinnode': par_fit[3]}
    else:
        par_fit_dict = {'shrinkage': par_fit[0],
                        'interaction.depth': par_fit[1],
                        'n.trees': par_fit[2]}

    par_fit_df = DataFrame(data=par_fit_dict, index=[0])
    best_pars_file = out_dir + '/best_pars/best_pars_' + str(jobnum) + '.csv'
    par_fit_df.to_csv(best_pars_file, index=False)


if __name__ == '__main__':
    print sys.argv  # for debugging

    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('bounds_file')
    parser.add_argument('out_dir')
    parser.add_argument('data_file')
    parser.add_argument('optimizer')
    parser.add_argument('learner')
    parser.add_argument('cv_folds', type=int)
    parser.add_argument('n_calls', type=int)
    parser.add_argument('jobnum')
    parser.add_argument('col_start', type=int)
    args = parser.parse_args()

    main(bounds_file=args.bounds_file,
         out_dir=args.out_dir,
         data_file=args.data_file,
         optimizer=args.optimizer,
         learner=args.learner,
         cv_folds=args.cv_folds,
         n_calls=args.n_calls,
         jobnum=args.jobnum,
         col_start=args.col_start)
