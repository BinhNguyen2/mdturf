#' Install the package 'combinat' if it is unavailable


if (!require('combinat')) install.packages('combinat'); library('combinat')

#' Calculate preference shares of items at the respondent level and at the aggregate level, and rank them at the aggregate level
#'
#' Technically this returns the matrix of preference shares of items at the individual level,
#' their preference shares and ranking at the aggregate level. Their preference shares and ranking
#' at the aggregate level are saved in the 2nd last row and the last row, respectively.
#' MaxDiff utilities at the respondent level, the number of items in a choice card are inputted. 
#' If weighting is required, the weighting matrix is also included.
#'
#' @inheritParams df is a dataframe of MaxDiff utilities, choice_item is the number of items in a choice card, and weight is the weighting matrix.
#' @return a matrix of preference shares at the respondent level as well as the aggregate level.
#' @seealso
#' @export
#' @examples
#' PreShare()

PreShare = function(df,choice_item,weight = NULL){
  if(is.null(weight) == TRUE){
    df_2 <- df[,1:ncol(df)]-rowMeans(df[,1:ncol(df)])
    df_3 <- exp(df_2)
    df_4 <- df_3/(df_3+(choice_item - 1))
    row.sums <- apply(df_4, 1, sum)
    df_5 <- df_4/row.sums
    df_5_Final = rbind.data.frame(df_5,colMeans(df_5),rank(colMeans(df_5)))
    return(df_5_Final) 
  }
  else {
    df_2 <- df[,1:ncol(df)]-rowMeans(df[,1:ncol(df)])
    df_3 <- exp(df_2)
    df_4 <- df_3/(df_3+(choice_item - 1))
    row.sums <- apply(df_4, 1, sum)
    df_5 <- df_4/row.sums
    df_6 <- df_5*drop(as.matrix(weight))
    df_6_Final = rbind.data.frame(df_5,colMeans(df_6),rank(colMeans(df_6)))
    return(df_6_Final)
  } 
}


#' Calculate a matrix of MaxDiff utilities which are mean-centred and exponentiated
#'
#' Technically this returns the maxtrix of MaxDiff utilities which are mean-centred and exponentiated.
#' MaxDiff utilities at the respondent level are inputted. There are 2 calculation steps:
#' computing mean-centred utilities and after that exponentiating these scores.
#'
#' @inheritParams df
#' @return a matrix of MaxDiff utilities which are mean-centred and exponentiated.
#' @seealso
#' @export
#' @examples
#' ExpMeanCent()

ExpMeanCent = function(df){
  df_2 <- df[,1:ncol(df)]-rowMeans(df[,1:ncol(df)])
  df_3 <- exp(df_2)
  return(df_3)
}

#' Generate all possible combinations
#'
#' Technically this returns the maxtrix of possible combinations provided the number of items and the combination size.
#' The number of items and the combination size are inputted.
#'
#' @inheritParams exp_mean_cent is the number of items and size is the portfolio size.
#' @return a matrix of possible combinations.
#' @seealso
#' @export
#' @examples
#' Combin()

Combin = function(exp_mean_cent,size){
  Y = combn(ncol(exp_mean_cent),size,fun = NULL, simplify = TRUE)
  Z = matrix(0L, nrow = ncol(exp_mean_cent), ncol = ncol(Y))
  for(c in 1:ncol(Y))
    for(r in 1:nrow(Y))
      Z[Y[r,c],c] = 1
  return(Z)
}

#' Calculate reaching probabilities of possible combinations
#'
#' Technically this returns the matrix of reaching probabilities of possible combinations,
#' their reaching probabilities and ranking at the aggregate level. Their reaching
#' probabilities and ranking are saved in the second last row and the last row, respectively. 
#' The maxtrix of the exponential mean-centred utilities and the number of items in a choice card are inputted.
#' Also, if weighting is required, the weighting matrix is included.
#'
#' @inheritParams exp_mean_cent is the exponentiated mean-centred utilities for individual items
#'  combi is the maxtrix of possible combinations of items and
#'  choice_item is the number of items in a choice card
#'  weight is a matrix of observation weights.
#' @return a matrix of reaching probabilities of possible combinations.
#' @seealso
#' @export
#' @examples
#' ReachProb()


ReachProb = function(exp_mean_cent,combi,choice_item,weight = NULL){
  if(is.null(weight) == TRUE){
    P_2 = as.matrix(exp_mean_cent) %*% combi
    PR_2 = P_2/(P_2+(choice_item-1))
    PR_2_column.means <- colMeans(PR_2)
    PR_2_Final = rbind.data.frame(PR_2,PR_2_column.means,rank(PR_2_column.means))
    return(PR_2_Final) 
  }
  else {
    P_2 = as.matrix(exp_mean_cent) %*% combi
    PR_2 = P_2/(P_2+(choice_item-1))
    PR_3 <- PR_2*drop(as.matrix(weight))
    PR_3_column.means <- apply(PR_3, 2, mean)
    PR_3_Final = rbind.data.frame(PR_2,PR_3_column.means,rank(PR_3_column.means))
    return(PR_3_Final)
  } 
}

#' Support to locate the combination with the n-highest reaching probability
#'
#' Technically this returns the combination with the n-highest reaching probability.
#' The maxtrix of exponentiated mean-centred utilities for possible combinations and the rank you need to extract are inputted.
#'
#' @inheritParams df is the dataframe ofexponentiated mean-centred utilities for possible combinations
#'  n is the rank of the combination to search for.
#' @return the combination number with the n-highest reaching probability
#' @seealso
#' @export
#' @examples
#' Rank()

Rank <- function(n) function(x) order(x, decreasing = TRUE)[n]

#' Search for the position of the combination with the n-highest reaching probability
#'
#' Technically this returns the combination with the n-highest reaching probability.
#' The maxtrix of exponentiated mean-centred utilities for possible combinations and the rank you need to extract are inputted.
#'
#' @inheritParams df is the dataframe ofexponentiated mean-centred utilities for possible combinations
#'  n is the rank of the combination to search for.
#' @return the combination number with the n-highest reaching probability
#' @seealso
#' @export
#' @examples
#' RankPos()


RankPos <- function(df,n){
   apply(df[nrow(df)-1,], 1, Rank(n))
}

#' Locate the 5 winning combinations with the 5 highest reaching probabilities
#'
#' Technically this returns the 5 winning combinations with the corresponding 5 highest reaching probabilities.
#' The maxtrix of exponentiated mean-centred utilities for possible combinations is inputted.
#'
#' @inheritParams df is the dataframe ofexponentiated mean-centred utilities for possible combinations
#' @return the positions of the 5 combinations with the 5 highest reaching probabilities
#' @seealso
#' @export
#' @examples
#' TopPos()

# Positions of top 5
TopPos <- function(df) {
  c(RankPos(df,1), RankPos(df,2), RankPos(df,3), RankPos(df,4), RankPos(df,5))
}

#' Extract the reaching probabilities of the 5 winning combinations
#'
#' Technically this returns the 5 corresponding reaching probabilities of 5 winning combinations.
#' The maxtrix of exponentiated mean-centred utilities for possible combinations is inputted.
#'
#' @inheritParams df is the dataframe ofexponentiated mean-centred utilities for possible combinations
#'  rank_pos_1,rank_pos_2,rank_pos_3,rank_pos_4,rank_pos_5 are the positions of the 5 winning combinations.
#' @return the reaching probabilities of the 5 winning combinations
#' @seealso
#' @export
#' @examples
#' TopUti()

# Utilities of top 5 portfolios
TopUti <- function(df, rank_pos_1,rank_pos_2,rank_pos_3,rank_pos_4,rank_pos_5){
  df[nrow(df)-1,c(rank_pos_1,rank_pos_2,rank_pos_3,rank_pos_4,rank_pos_5)]
}


