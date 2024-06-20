#' pairs_findr
#'
#' Takes a character as input and detect the pairs of pattern, like the parenthesis pais if the pattern is "(" and then ")"
#'
#' @param inpt is the input character
#' @param ptrn1 is the first pattern ecountered in the pair
#' @param ptrn2 is the second pattern in the pair
#' @examples
#' 
#' print(pairs_findr(inpt="ze+(yu*45/(jk+zz)*(o()p))-(re*(rt+qs)-fg)"))
#' 
#' [[1]]
#'  [1] 4 1 1 3 2 2 3 4 6 5 5 6
#' 
#' [[2]]
#'  [1]  4 11 17 19 21 22 24 25 27 31 37 41
#'
#' @export

pairs_findr <- function(inpt, ptrn1="(", ptrn2=")"){

        regex_spe_detect <- function(inpt){

                fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
                  
                  ptrn <- grep(ptrn_fill, inpt_v)

                  while (length(ptrn) > 0){
                   
                    ptrn <- grep(ptrn_fill, inpt_v)

                    idx <- ptrn[1] 
                    
                    untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
                   
                    pre_val <- inpt_v[(idx - 1)]

                    inpt_v[idx] <- pre_val

                    if (untl > 0){
                    
                      for (i in 1:untl){
                        
                        inpt_v <- append(inpt_v, pre_val, idx)
                        
                      }
                      
                    }

                  ptrn <- grep(ptrn_fill, inpt_v)
                    
                  }
                  
                  return(inpt_v)
                  
                }

           inpt <- unlist(strsplit(x=inpt, split=""))

           may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")

           pre_idx <- unique(match(x=inpt, table=may_be_v))

           pre_idx <- pre_idx[!(is.na(pre_idx))]

           for (el in may_be_v[pre_idx]){

                   for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){

                           inpt <- append(x=inpt, values="\\", after=(i-1))

                   }

           }

        
           return(paste(inpt, collapse=""))

        }

        lst <- unlist(strsplit(x=inpt, split=""))

        lst_par <- c()

        lst_par_calc <- c()

        lst_pos <- c()

        paires = 1

        pre_paires = 1

        pre_paires2 = 1

        if ((length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2) > 0){

                for (i in 1:(length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2)){ 

                        lst_par <- c(lst_par, 0)

                        lst_par_calc <- c(lst_par_calc, 0)

                        lst_pos <- c(lst_pos, 0)


                }

        }

        vec_ret <- c()

        par_ = 1

        lvl_par = 0

        for (el in 1:length(lst)){

           if (lst[el] == ptrn1){

                   if (!(is.null(vec_ret))){

                           lst_par_calc[pre_paires2:pre_paires][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] + 1

                   }else{

                           lst_par_calc[pre_paires2:pre_paires] <- lst_par_calc[pre_paires2:pre_paires] + 1

                   }

                   pre_paires = pre_paires + 1

                   pre_cls <- TRUE

                   lst_pos[par_] <- el

                   par_ = par_ + 1

                   lvl_par = lvl_par + 1

           }

           if (lst[el] == ptrn2){

                   lvl_par = lvl_par - 1

                   if (!(is.null(vec_ret))){

                        lst_par_calc[c(pre_paires2:pre_paires)][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] - 1

                        pre_val <- lst_par_calc[pre_paires2:pre_paires][vec_ret]

                        lst_par_calc[pre_paires2:pre_paires][vec_ret] <- (-2)
                   
                   }else{

                        lst_par_calc[c(pre_paires2:pre_paires)] <- lst_par_calc[pre_paires2:pre_paires] - 1

                   }

                   if (!(is.null(vec_ret))){ 

                           pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])

                           lst_par_calc[pre_paires2:pre_paires][vec_ret] <- pre_val 

                   }else{

                           pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires])

                   }

                   cnt_par = 1

                   cnt2 = 0

                   if (!(is.null(vec_ret))){

                           vec_ret <- sort(vec_ret)

                           if (pre_mtch[1] >= min(vec_ret)){

                                cnt2 = 2

                                while (pre_mtch[1] > cnt_par & cnt2 <= length(vec_ret)){

                                        if ((vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) > 1){

                                                cnt_par = cnt_par + (vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) - 1

                                        }

                                        cnt2 = cnt2 + 1

                                }

                                if (pre_mtch[1] > cnt_par){

                                        cnt_par = length(vec_ret) / 2 + 1

                                }

                                cnt2 = cnt2 - 1

                           }

                   }

                   lst_par[pre_mtch[1] + (pre_paires2 - 1) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1))] <- paires 

                   lst_par[pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret)] <- paires 

                   if ((pre_mtch[1] + (pre_paires2 - 1)) == 1){

                        pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1

                        vec_ret <- c()

                        cnt_par = 0

                   } else if (lst_par_calc[(pre_mtch[1] + (pre_paires2 - 1) - 1)] == -1 & ifelse(is.null(vec_ret), TRUE, 
                                is.na(match(x=-1, table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])))){

                        pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1

                        vec_ret <- c()

                        cnt_par = 0

                   } else{

                        vec_ret <- c(vec_ret, (pre_mtch[1]) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1)), 
                                     (pre_mtch[2] + length(vec_ret)))

                   }

                   paires = paires + 1

                   pre_paires = pre_paires + 1

                   pre_cls <- FALSE

                   lst_pos[par_] <- el

                   par_ = par_ + 1

           }

        }

        return(list(lst_par, lst_pos))

}

#' pairs_findr_merger
#'
#' Takes two different outputs from pairs_findr and merge them. Can be usefull when the pairs consists in different patterns, for example one output from the pairs_findr function with ptrn1 = "(" and ptrn2 = ")", and a second output from the pairs_findr function with ptrn1 = "[" and ptrn2 = "]".
#'
#' @param lst1 is the first ouput from pairs findr function
#' @param lst2 is the second ouput from pairs findr function
#' @examples
#'
#' print(pairs_findr_merger(lst1=list(c(1, 2, 3, 3, 2, 1), c(3, 4, 5, 7, 8, 9)), 
#'                          lst2=list(c(1, 1), c(1, 2))))
#' 
#' [[1]]
#' [1] 1 1 2 3 4 4 3 2
#' 
#' [[2]]
#' [1] 1 2 3 4 5 7 8 9
#' 
#' print(pairs_findr_merger(lst1=list(c(1, 2, 3, 3, 2, 1), c(3, 4, 5, 7, 8, 9)), 
#'                          lst2=list(c(1, 1), c(1, 11))))
#' 
#' [[1]]
#' [1] 1 2 3 4 4 3 2 1
#' 
#' [[2]]
#' [1]  1  3  4  5  7  8  9 11
#' 
#' print(pairs_findr_merger(lst1=list(c(1, 2, 3, 3, 2, 1), c(3, 4, 5, 8, 10, 11)), 
#'                          lst2=list(c(4, 4), c(6, 7))))
#'
#' [[1]]
#' [1] 1 2 3 4 4 3 2 1
#' 
#' [[2]]
#' [1]  3  4  5  6  7  8 10 11
#' 
#' print(pairs_findr_merger(lst1=list(c(1, 2, 3, 3, 2, 1), c(3, 4, 5, 7, 10, 11)), 
#'                          lst2=list(c(4, 4), c(8, 9))))
#' 
#' [[1]]
#' [1] 1 2 3 3 4 4 2 1
#' 
#' [[2]]
#' [1]  3  4  5  7  8  9 10 11
#' 
#' print(pairs_findr_merger(lst1=list(c(1, 2, 3, 3, 2, 1), c(3, 4, 5, 7, 10, 11)), 
#'                          lst2=list(c(4, 4), c(18, 19))))
#'
#' [[1]]
#' [1] 1 2 3 3 2 1 4 4
#' 
#' [[2]]
#' [1]  3  4  5  7 10 11 18 19
#'
#' print(pairs_findr_merger(lst1 = list(c(1, 1, 2, 2, 3, 3), c(1, 25, 26, 32, 33, 38)), 
#'                         lst2 = list(c(1, 1, 2, 2, 3, 3), c(7, 11, 13, 17, 19, 24))))
#'
#' [[1]]
#'  [1] 1 2 2 3 3 4 4 1 5 5 6 6
#' 
#' [[2]]
#'  [1]  1  7 11 13 17 19 24 25 26 32 33 38
#'
#' print(pairs_findr_merger(lst1 = list(c(1, 1, 2, 2, 3, 3), c(2, 7, 9, 10, 11, 15)), 
#'                          lst2 = list(c(3, 2, 1, 1, 2, 3, 4, 4), c(1, 17, 18, 22, 23, 29, 35, 40))))
#'
#' [[1]]
#'  [1] 6 5 1 1 2 2 3 3 4 4 5 6 7 7
#' 
#' [[2]]
#'  [1]  1  2  7  9 10 11 15 17 18 22 23 29 35 40
#'
#' print(pairs_findr_merger(lst1 = list(c(1, 1), c(22, 23)), 
#'                          lst2 = list(c(1, 1, 2, 2), c(3, 21, 27, 32))))
#'
#' [[1]]
#' [1] 1 1 2 2 3 3
#' 
#' [[2]]
#' [1]  3 21 22 23 27 32
#'
#' @export

pairs_findr_merger <- function(lst1=list(), lst2=list()){
    better_match <- function(inpt_v=c(), ptrn, untl=1, nvr_here=NA){
      Rtn_v <- c()
      if (length(untl) < length(ptrn)){
        val_add <- untl[length(untl)]
        while (length(untl) < length(ptrn)){
          untl <- c(untl, val_add)
        }
      }
      for (cur_ptrn in 1:length(ptrn)){
        rtn_v <- c()
        cnt = 1
        stop <- FALSE
        while (length(rtn_v) < untl[cur_ptrn] & cnt < (length(inpt_v) + 1) & !(stop)){
                pre_match <- match(x=ptrn[cur_ptrn], table=inpt_v)
                if (!(is.na(pre_match))){
                  inpt_v[pre_match] <- nvr_here
                  rtn_v <- c(rtn_v, pre_match)
                }else{
                  stop <- TRUE
                }
                cnt = cnt + 1
        }
        Rtn_v <- c(Rtn_v, rtn_v)
      }
      return(Rtn_v)
    }
    pair1 <- unlist(lst1[1])
    pos1 <- unlist(lst1[2])
    pair2 <- unlist(lst2[1])
    pos2 <- unlist(lst2[2])
    stop <- FALSE
    cnt = 1
    while (!(stop)){
      mtch1 <- match(x = cnt, table = pair1)
      mtch2 <- match(x = cnt, table = pair2)
      if (all(!(is.na(mtch1)), !(is.na(mtch2)))){
        if (pos1[mtch1] < pos2[mtch2]){
          poses <- better_match(inpt_v = pair2, ptrn = c(cnt:max(pair2)), untl = 2)
          pair2[poses] <- pair2[poses] + 1
        }else{
          poses <- better_match(inpt_v = pair1, ptrn = c(cnt:max(pair1)), untl = 2)
          pair1[poses] <- pair1[poses] + 1
        }
      }else{
        stop <- TRUE
      }
      cnt = cnt + 1
    }
    if (length(pair1) > length(pair2)){
      rtn_pos <- pos1
      rtn_pair <- pair1
      add_pos <- pos2
      add_pair <- pair2
    }else{
      rtn_pos <- pos2
      rtn_pair <- pair2
      add_pos <- pos1
      add_pair <- pair1
    }
    cnt = 1
    stop <- FALSE
    pre_lngth <- length(rtn_pos)
    while (cnt <= (pre_lngth / 2 + length(add_pair) / 2) & !(stop)){
      if (is.na(match(x = cnt, table = rtn_pair))){
          cur_add_pos_id <- grep(x = add_pair, pattern = cnt)
          if (cnt < max(rtn_pair)){
            incr = 1
            cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
            while (identical(integer(0), cur_grep)){
                incr = incr + 1
                cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
            }
            if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[2]] & 
                rtn_pos[cur_grep[1]] > add_pos[cur_add_pos_id[1]]){
              rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
              cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[2]])
              cur_pos <- which.min(cur_vec)
              rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
            }else{
              if (!(is.na(match(x = (cnt - 1), table = rtn_pair)))){
                    cur_grep2 <- grep(x = rtn_pair, pattern = (cnt - 1))
                    if (rtn_pos[cur_grep2[2]] > add_pos[cur_add_pos_id[2]]){
                      rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep2[1])
                      rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep2[1] + 1))
                      rtn_pos <- append(x = rtn_pos, 
                                        value = add_pos[cur_add_pos_id[1]], after = cur_grep2[1])
                      rtn_pos <- append(x = rtn_pos, 
                                        value = add_pos[cur_add_pos_id[2]], after = (cur_grep2[1] + 1))
                    }else{
                      rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                      rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                      rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                      rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
                    }
              }else{
                rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
              }
            }
          }else{
            incr = 1
            cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
            while (identical(integer(0), cur_grep)){
              incr = incr + 1
              cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
            }
            if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[1]]){
              cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[1]])
              cur_pos <- which.min(cur_vec)
              rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_pos)
              rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_pos)
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
            }else{
              rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
              rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_grep[1])
              rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = cur_grep[1])
            }
          }
      }
      cnt = cnt + 1
    }
    return(list(rtn_pair, sort(rtn_pos)))
}

#' depth_pairs_findr
#'
#' Takes the pair vector as an input and associate to each pair a level of depth, see examples
#'
#' @param inpt is the pair vector
#' @examples 
#'
#' print(depth_pairs_findr(c(1, 1, 2, 3, 3, 4, 4, 2, 5, 6, 7, 7, 6, 5)))
#'
#'  [1] 1 1 1 2 2 2 2 1 1 2 3 3 2 1
#'
#' @export

depth_pairs_findr <- function(inpt){
  rtn_v <- c(matrix(data = 0, nrow = length(inpt), ncol = 1))
  all_pair <- c(matrix(data = 0, nrow = length(unique(inpt)), ncol = 1))
  alrd_here <- c()
  cnt = 1
  cnt2 = 1
  while (cnt2 < length(rtn_v)){
    if (inpt[cnt2]  == inpt[(cnt2 + 1)]){
      rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
      cnt2 = cnt2 + 2
    }else if (!(is.na(match(x = inpt[cnt2], table = alrd_here)))){
      cnt = cnt - 1
      rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
      cnt2 = cnt2 + 1
    }else{
      cnt = cnt + 1
      alrd_here <- c(alrd_here, inpt[cnt2])
      cnt2 = cnt2 + 1
    }
  }
  if (rtn_v[length(rtn_v)] == 0){
    rtn_v[grep(x = rtn_v, pattern = 0)] <- 1
  }
  return(rtn_v)
}

#' pairs_insertr
#'
#' Takes a character representing an arbitrary condition (like ReGeX for example) or an information (to a parser for example), vectors containing all the pair of pattern that potentially surrounds condition (flagged_pair_v and corr_v), and a vector containing all the conjuntion character, as input and returns the character with all or some of the condition surrounded by the pair characters. See examples. All the pair characters are inserted according to the closest pair they found priotizing those found next to the condition and on the same depth-level and , if not found, the pair found at the n+1 depth-level.
#'
#' @param inpt is the input character representing an arbitrary condition, like ReGex for example, or information to a parser for example
#' @param algo_used is a vector containing one or more of the 3 algorythms used. The first algorythm will simply put the pair of parenthesis at the condition surrounded and/or after a character flagged (in flagged_conj_v) as a conjunction. The second algorythm will put parenthesis at the condition that are located after other conditions that are surrounded by a pair. The third algorythm will put a pair at all the condition, it is very powerfull but takes a longer time. See examples and make experience to see which combination of algorythm(s) is the most efficient for your use case.
#' @param flagged_pair_v is a vector containing all the first character of the pairs
#' @param corr_v is a vector containing all the last character of the pairs
#' @param flagged_conj_v is a vector containing all the conjunction character
#' @examples
#'
#' print(pairs_insertr(inpt = "([one]|two|twob)three(four)", algo_used = c(1)))
#' 
#' [1] "([one]|[two]|[twob])three(four)"
#'
#' print(pairs_insertr(inpt = "(one|[two]|twob)three(four)", algo_used = c(2)))
#'
#' [1] "(one|[two]|[twob])(three)(four)"
#' 
#' print(pairs_insertr(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2)))
#'
#' [1] "(oneA|[one]|[two]|[twob])(three)(four)"
#'
#' print(pairs_insertr(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2, 3)))
#'
#' [1] "([oneA]|[one]|[two]|[twob])(three)(four)"
#'
#' print(pairs_insertr(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(3)))
#'
#' [1] "([oneA]|[one]|(two)|(twob))(three)(four)"
#' 
#' print(pairs_insertr(inpt = "(oneA|[one]|two|twob)three((four))", algo_used = c(3)))
#' 
#' [1] "([oneA]|[(one)]|(two)|(twob))(three)((four))"
#'
#' @export

pairs_insertr <- function(inpt, algo_used = c(1:3), flagged_pair_v = c(")", "]"), corr_v = c("(", "["), flagged_conj_v = c("&", "|")){
  inpt <- unlist(strsplit(x = inpt, split = ""))
  cur_vec <- c(flagged_pair_v, flagged_conj_v, corr_v)
  if (1 %in% algo_used){
    frst_val <- "("
    scd_val <- ")"
    cnt = 1
    while (cnt < length(inpt)){
      mtch_pair <- match(x = inpt[cnt], table = corr_v)
      if (!(is.na(mtch_pair))){ 
        frst_val <- corr_v[mtch_pair]
        scd_val <- flagged_pair_v[mtch_pair]
      }
      if (inpt[cnt] %in% flagged_conj_v & is.na(match(x = inpt[(cnt + 1)] , table = corr_v))){
        inpt <- append(x = inpt, value = frst_val, after = cnt)
        cnt = cnt + 2
        stop <- FALSE
        while (cnt < length(inpt) & !(stop)){
          if (is.na(match(x = inpt[cnt], table = cur_vec))){
            cnt = cnt + 1
          }else{
            stop <- TRUE
          }
        }
        inpt <- append(x = inpt, value = scd_val, after = (cnt - 1))
      }
      cnt = cnt + 1
    }
  }
  if (2 %in% algo_used){
    cnt = 1
    while (cnt < length(inpt)){
      cur_mtch <- match(table = flagged_pair_v, x = inpt[cnt])
      if (!(is.na(cur_mtch)) & is.na(match(x = inpt[(cnt + 1)], table = flagged_pair_v))){
          if (!(is.na(match(x = inpt[(cnt + 1)], table = flagged_conj_v)))){
            cnt = cnt + 1
          } 
         if (is.na(match(x = inpt[(cnt + 1)], table = corr_v))){
            inpt <- append(x = inpt, value = corr_v[cur_mtch[1]], after = cnt)
            cnt = cnt + 2
            stop <- FALSE
            while (cnt <= length(inpt) & !(stop)){
              if (is.na(match(x = inpt[cnt], table = cur_vec))){
                cnt = cnt + 1
              }else{
                stop <- TRUE
              }
            }
            inpt <- append(x = inpt, value = flagged_pair_v[cur_mtch[1]], after = (cnt - 1))
          }
      }
      cnt = cnt + 1
    }
  }
  if (3 %in% algo_used){
    depth_pairs_findr <- function(inpt){
        rtn_v <- c(matrix(data = 0, nrow = length(inpt), ncol = 1))
        all_pair <- c(matrix(data = 0, nrow = length(unique(inpt)), ncol = 1))
        alrd_here <- c()
        cnt = 1
        cnt2 = 1
        while (cnt2 < length(rtn_v)){
          if (inpt[cnt2]  == inpt[(cnt2 + 1)]){
            rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
            cnt2 = cnt2 + 2
          }else if (!(is.na(match(x = inpt[cnt2], table = alrd_here)))){
            cnt = cnt - 1
            rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
            cnt2 = cnt2 + 1
          }else{
            cnt = cnt + 1
            alrd_here <- c(alrd_here, inpt[cnt2])
            cnt2 = cnt2 + 1
          }
        }
        if (rtn_v[length(rtn_v)] == 0){
          rtn_v[grep(x = rtn_v, pattern = 0)] <- 1
        }
        return(rtn_v)
      }
    pairs_findr <- function(inpt, ptrn1="(", ptrn2=")"){
      regex_spe_detect <- function(inpt){
              fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
                ptrn <- grep(ptrn_fill, inpt_v)
                while (length(ptrn) > 0){
                  ptrn <- grep(ptrn_fill, inpt_v)
                  idx <- ptrn[1] 
                  untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
                  pre_val <- inpt_v[(idx - 1)]
                  inpt_v[idx] <- pre_val
                  if (untl > 0){
                    for (i in 1:untl){
                      inpt_v <- append(inpt_v, pre_val, idx)
                    }
                  }
                ptrn <- grep(ptrn_fill, inpt_v)
                }
                return(inpt_v)
              }
         inpt <- unlist(strsplit(x=inpt, split=""))
         may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
         pre_idx <- unique(match(x=inpt, table=may_be_v))
         pre_idx <- pre_idx[!(is.na(pre_idx))]
         for (el in may_be_v[pre_idx]){
                 for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                         inpt <- append(x=inpt, values="\\", after=(i-1))
                 }
         }
         return(paste(inpt, collapse=""))
      }
      lst <- unlist(strsplit(x=inpt, split=""))
      lst_par <- c()
      lst_par_calc <- c()
      lst_pos <- c()
      paires = 1
      pre_paires = 1
      pre_paires2 = 1
      if ((length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2) > 0){
              for (i in 1:(length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2)){ 
                      lst_par <- c(lst_par, 0)
                      lst_par_calc <- c(lst_par_calc, 0)
                      lst_pos <- c(lst_pos, 0)
              }
      }
      vec_ret <- c()
      par_ = 1
      lvl_par = 0
      for (el in 1:length(lst)){
         if (lst[el] == ptrn1){
                 if (!(is.null(vec_ret))){
                         lst_par_calc[pre_paires2:pre_paires][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] + 1
                 }else{
                         lst_par_calc[pre_paires2:pre_paires] <- lst_par_calc[pre_paires2:pre_paires] + 1
                 }
                 pre_paires = pre_paires + 1
                 pre_cls <- TRUE
                 lst_pos[par_] <- el
                 par_ = par_ + 1
                 lvl_par = lvl_par + 1
         }
         if (lst[el] == ptrn2){
                 lvl_par = lvl_par - 1
                 if (!(is.null(vec_ret))){
                      lst_par_calc[c(pre_paires2:pre_paires)][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] - 1
                      pre_val <- lst_par_calc[pre_paires2:pre_paires][vec_ret]
                      lst_par_calc[pre_paires2:pre_paires][vec_ret] <- (-2)
                 }else{
                      lst_par_calc[c(pre_paires2:pre_paires)] <- lst_par_calc[pre_paires2:pre_paires] - 1
                 }
                 if (!(is.null(vec_ret))){ 
                         pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])
                         lst_par_calc[pre_paires2:pre_paires][vec_ret] <- pre_val 
                 }else{
                         pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires])
                 }
                 cnt_par = 1
                 cnt2 = 0
                 if (!(is.null(vec_ret))){
                         vec_ret <- sort(vec_ret)
                         if (pre_mtch[1] >= min(vec_ret)){
                              cnt2 = 2
                              while (pre_mtch[1] > cnt_par & cnt2 <= length(vec_ret)){
                                      if ((vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) > 1){
                                              cnt_par = cnt_par + (vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) - 1
                                      }
                                      cnt2 = cnt2 + 1
                              }
                              if (pre_mtch[1] > cnt_par){
                                      cnt_par = length(vec_ret) / 2 + 1
                              }
                              cnt2 = cnt2 - 1
                         }
                 }
                 lst_par[pre_mtch[1] + (pre_paires2 - 1) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1))] <- paires 
                 lst_par[pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret)] <- paires 
                 if ((pre_mtch[1] + (pre_paires2 - 1)) == 1){
                      pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
                      vec_ret <- c()
                      cnt_par = 0
                 } else if (lst_par_calc[(pre_mtch[1] + (pre_paires2 - 1) - 1)] == -1 & ifelse(is.null(vec_ret), TRUE, 
                              is.na(match(x=-1, table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])))){

                      pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
                      vec_ret <- c()
                      cnt_par = 0
                 } else{
                      vec_ret <- c(vec_ret, (pre_mtch[1]) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1)), 
                                   (pre_mtch[2] + length(vec_ret)))
                 }
                 paires = paires + 1
                 pre_paires = pre_paires + 1
                 pre_cls <- FALSE
                 lst_pos[par_] <- el
                 par_ = par_ + 1
         }
      }
      return(list(lst_par, lst_pos))
      }
      pairs_findr_merger <- function(lst1=list(), lst2=list()){
        better_match <- function(inpt_v=c(), ptrn, untl=1, nvr_here=NA){
          Rtn_v <- c()
          if (length(untl) < length(ptrn)){
            val_add <- untl[length(untl)]
            while (length(untl) < length(ptrn)){
              untl <- c(untl, val_add)
            }
          }
          for (cur_ptrn in 1:length(ptrn)){
            rtn_v <- c()
            cnt = 1
            stop <- FALSE
            while (length(rtn_v) < untl[cur_ptrn] & cnt < (length(inpt_v) + 1) & !(stop)){
                    pre_match <- match(x=ptrn[cur_ptrn], table=inpt_v)
                    if (!(is.na(pre_match))){
                      inpt_v[pre_match] <- nvr_here
                      rtn_v <- c(rtn_v, pre_match)
                    }else{
                      stop <- TRUE
                    }
                    cnt = cnt + 1
            }
            Rtn_v <- c(Rtn_v, rtn_v)
          }
          return(Rtn_v)
        }
        pair1 <- unlist(lst1[1])
        pos1 <- unlist(lst1[2])
        pair2 <- unlist(lst2[1])
        pos2 <- unlist(lst2[2])
        stop <- FALSE
        cnt = 1
        while (!(stop)){
          mtch1 <- match(x = cnt, table = pair1)
          mtch2 <- match(x = cnt, table = pair2)
          if (all(!(is.na(mtch1)), !(is.na(mtch2)))){
            if (pos1[mtch1] < pos2[mtch2]){
              poses <- better_match(inpt_v = pair2, ptrn = c(cnt:max(pair2)), untl = 2)
              pair2[poses] <- pair2[poses] + 1
            }else{
              poses <- better_match(inpt_v = pair1, ptrn = c(cnt:max(pair1)), untl = 2)
              pair1[poses] <- pair1[poses] + 1
            }
          }else{
            stop <- TRUE
          }
          cnt = cnt + 1
        }
        if (length(pair1) > length(pair2)){
          rtn_pos <- pos1
          rtn_pair <- pair1
          add_pos <- pos2
          add_pair <- pair2
        }else{
          rtn_pos <- pos2
          rtn_pair <- pair2
          add_pos <- pos1
          add_pair <- pair1
        }
        cnt = 1
        stop <- FALSE
        pre_lngth <- length(rtn_pos)
        while (cnt <= (pre_lngth / 2 + length(add_pair) / 2) & !(stop)){
          if (is.na(match(x = cnt, table = rtn_pair))){
              cur_add_pos_id <- grep(x = add_pair, pattern = cnt)
              if (cnt < max(rtn_pair)){
                incr = 1
                cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
                while (identical(integer(0), cur_grep)){
                    incr = incr + 1
                    cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
                }
                if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[2]] & 
                    rtn_pos[cur_grep[1]] > add_pos[cur_add_pos_id[1]]){
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                  cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[2]])
                  cur_pos <- which.min(cur_vec)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
                }else{
                  if (!(is.na(match(x = (cnt - 1), table = rtn_pair)))){
                        cur_grep2 <- grep(x = rtn_pair, pattern = (cnt - 1))
                        if (rtn_pos[cur_grep2[2]] > add_pos[cur_add_pos_id[2]]){
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep2[1])
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep2[1] + 1))
                          rtn_pos <- append(x = rtn_pos, 
                                            value = add_pos[cur_add_pos_id[1]], after = cur_grep2[1])
                          rtn_pos <- append(x = rtn_pos, 
                                            value = add_pos[cur_add_pos_id[2]], after = (cur_grep2[1] + 1))
                        }else{
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                          rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                          rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
                        }
                  }else{
                    rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                    rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                    rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                    rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
                  }
                }
              }else{
                incr = 1
                cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
                while (identical(integer(0), cur_grep)){
                  incr = incr + 1
                  cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
                }
                if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[1]]){
                  cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[1]])
                  cur_pos <- which.min(cur_vec)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_pos)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_pos)
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
                }else{
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_grep[1])
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = cur_grep[1])
                }
              }
          }
          cnt = cnt + 1
        }
        return(list(rtn_pair, sort(rtn_pos)))
      }
    cur_lst <-  pairs_findr(inpt = inpt, ptrn1 = corr_v[1], ptrn2 = flagged_pair_v[1])
    if (length(corr_v) > 1){
      for (pair in 2:length(corr_v)){
        cur_fun <- pairs_findr(inpt = inpt, ptrn1 = corr_v[pair], ptrn2 = flagged_pair_v[pair]) 
        cur_lst <- pairs_findr_merger(lst1 = cur_lst, lst2 = cur_fun)
      }
    }
    lst_pair <- unlist(cur_lst[1])
    lst_pos <- unlist(cur_lst[2])
    cur_depth <- depth_pairs_findr(inpt = unlist(cur_lst[1])) 
    frst_val <- "("
    scd_val <- ")"
    par = 2
    while (par <= length(lst_pair)){
      mtch_pair <- match(x = inpt[lst_pos[par]], table = corr_v)
      if (!(is.na(mtch_pair))){
        frst_val <- corr_v[mtch_pair] 
        scd_val <- flagged_pair_v[mtch_pair]
      }
      if (lst_pair[(par - 1)] != lst_pair[par] & abs(lst_pos[(par - 1)] - lst_pos[par]) > 1){
        cnt = lst_pos[(par - 1)]
        ahd <- TRUE
        if (!(is.na(match(x = inpt[(lst_pos[(par - 1)] + 1)], table = flagged_conj_v)))){
          if (abs(lst_pos[(par - 1)] - lst_pos[par]) == 2){
            ahd <- FALSE
          }
          else{
            cnt = cnt + 1
          }
        }
        if (ahd){
          if (par < (length(cur_depth) - 1)){
            if (cur_depth[(par + 2)] == cur_depth[(par + 1)]){
              mtch_pair <- match(x = inpt[lst_pos[(par + 1)]] , table = flagged_pair_v)
              if (is.na(mtch_pair)){
                mtch_pair <- match(x = inpt[lst_pos[(par + 1)]] , table = corr_v)
              }
              frst_val <- corr_v[mtch_pair]
              scd_val <- flagged_pair_v[mtch_pair]
            }
          }
          inpt <- append(x = inpt, value = frst_val, after = cnt)
          cnt = cnt + 2
          stop <- FALSE
          while (!(stop) & cnt <= length(inpt)){
            if (is.na(match(x = inpt[cnt], table = cur_vec))){
              cnt = cnt + 1
            }else{
              stop <- TRUE
            }
          }
          inpt <- append(x = inpt, value = scd_val, after = (cnt - 1))
          cur_lst <-  pairs_findr(inpt = inpt, ptrn1 = corr_v[1], ptrn2 = flagged_pair_v[1])
          if (length(corr_v) > 1){
             for (pair in 2:length(corr_v)){
               cur_fun <- pairs_findr(inpt = inpt, ptrn1 = corr_v[pair], ptrn2 = flagged_pair_v[pair]) 
               cur_lst <- pairs_findr_merger(lst1 = cur_lst, lst2 = cur_fun)
             }
          }
          lst_pair <- unlist(cur_lst[1])
          lst_pos <- unlist(cur_lst[2])
          cur_depth <- depth_pairs_findr(inpt = unlist(cur_lst[1]))
        }
      }
      par = par + 1
    }
  }
  return(paste(inpt, collapse = ""))
}


#' pairs_insertr2
#'
#' Takes a character representing an arbitrary condition (like ReGeX for example) or an information (to a parser for example), vectors containing all the pair of pattern that potentially surrounds condition (flagged_pair_v and corr_v), and a vector containing all the conjuntion character, as input and returns the character with all or some of the condition surrounded by the pair characters. See examples. All the pair characters are inserted according to the closest pair they found priotizing those found next to the condition and on the same depth-level and , if not found, the pair found at the n+1 depth-level.
#'
#' @param inpt is the input character representing an arbitrary condition, like ReGex for example, or information to a parser for example
#' @param algo_used is a vector containing one or more of the 3 algorythms used. The first algorythm will simply put the pair of parenthesis at the condition surrounded and/or after a character flagged (in flagged_conj_v) as a conjunction. The second algorythm will put parenthesis at the condition that are located after other conditions that are surrounded by a pair. The third algorythm will put a pair at all the condition, it is very powerfull but takes a longer time. See examples and make experience to see which combination of algorythm(s) is the most efficient for your use case.
#' @param flagged_pair_v is a vector containing all the first character of the pairs
#' @param corr_v is a vector containing all the last character of the pairs
#' @param flagged_conj_v is a vector containing all the conjunction character
#' @param method is length 2 vector containing as a first index, the first character of the pair inserted, and at the last index, the second and last character of the pair 
#' @examples
#'
#' print(pairs_insertr2(inpt = "([one]|two|twob)three(four)", algo_used = c(1), method = c("(", ")")))
#'
#' [1] "([one]|(two)|(twob))three(four)"
#'
#' print(pairs_insertr2(inpt = "([one]|two|twob)three(four)", algo_used = c(1), method = c("[", "]")))
#'
#' [1] "([one]|[two]|[twob])three(four)"
#'
#' print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2)))
#'
#' [1] "(oneA|[one]|(two)|(twob))(three)(four)"
#'
#' print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2), method = c("-", "#"),
#'                      flagged_pair_v = c(")", "]", "#"), corr_v = c("(", "[", "-")))
#'
#' [1] "(oneA|[one]|-two#|-twob#)-three#(four)"
#'
#' print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(1, 2, 3)))
#'
#' [1] "((oneA)|[one]|(two)|(twob))(three)(four)"
#'
#' print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three(four)", algo_used = c(3), method = c("[", "]")))
#'
#' [1] "([oneA]|[one]|[two]|[twob])[three](four)"
#'
#' print(pairs_insertr2(inpt = "(oneA|[one]|two|twob)three((four))", algo_used = c(3)))
#'
#' [1] "((oneA)|[one]|(two)|(twob))(three)((four))"
#'
#' @export

pairs_insertr2 <- function(inpt, algo_used = c(1:3), flagged_pair_v = c(")", "]"), corr_v = c("(", "["), flagged_conj_v = c("&", "|"),
                                                        method = c("(", ")")){
  inpt <- unlist(strsplit(x = inpt, split = ""))
  cur_vec <- c(flagged_pair_v, flagged_conj_v, corr_v)
  if (1 %in% algo_used){
    cnt = 1
    while (cnt < length(inpt)){
      if (inpt[cnt] %in% flagged_conj_v & is.na(match(x = inpt[(cnt + 1)] , table = corr_v))){
        inpt <- append(x = inpt, value = method[1], after = cnt)
        cnt = cnt + 2
        stop <- FALSE
        while (cnt < length(inpt) & !(stop)){
          if (is.na(match(x = inpt[cnt], table = cur_vec))){
            cnt = cnt + 1
          }else{
            stop <- TRUE
          }
        }
        inpt <- append(x = inpt, value = method[2], after = (cnt - 1))
      }
      cnt = cnt + 1
    }
  }
  if (2 %in% algo_used){
    cnt = 1
    while (cnt < length(inpt)){
      cur_mtch <- match(table = flagged_pair_v, x = inpt[cnt])
      if (!(is.na(cur_mtch)) & is.na(match(x = inpt[(cnt + 1)], table = flagged_pair_v))){
        if (!(is.na(match(x = inpt[(cnt + 1)], table = flagged_conj_v)))){
          cnt = cnt + 1
        } 
        if (is.na(match(x = inpt[(cnt + 1)], table = corr_v))){
           inpt <- append(x = inpt, value = method[1], after = cnt)
           cnt = cnt + 2
           stop <- FALSE
           while (cnt <= length(inpt) & !(stop)){
             if (is.na(match(x = inpt[cnt], table = cur_vec))){
               cnt = cnt + 1
             }else{
               stop <- TRUE
             }
           }
           inpt <- append(x = inpt, value = method[2], after = (cnt - 1))
         }
      }
      cnt = cnt + 1
    }
  }
  if (3 %in% algo_used){
    depth_pairs_findr <- function(inpt){
        rtn_v <- c(matrix(data = 0, nrow = length(inpt), ncol = 1))
        all_pair <- c(matrix(data = 0, nrow = length(unique(inpt)), ncol = 1))
        alrd_here <- c()
        cnt = 1
        cnt2 = 1
        while (cnt2 < length(rtn_v)){
          if (inpt[cnt2]  == inpt[(cnt2 + 1)]){
            rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
            cnt2 = cnt2 + 2
          }else if (!(is.na(match(x = inpt[cnt2], table = alrd_here)))){
            cnt = cnt - 1
            rtn_v[grep(x = inpt, pattern = inpt[cnt2])] <- cnt
            cnt2 = cnt2 + 1
          }else{
            cnt = cnt + 1
            alrd_here <- c(alrd_here, inpt[cnt2])
            cnt2 = cnt2 + 1
          }
        }
        if (rtn_v[length(rtn_v)] == 0){
          rtn_v[grep(x = rtn_v, pattern = 0)] <- 1
        }
        return(rtn_v)
      }
    pairs_findr <- function(inpt, ptrn1="(", ptrn2=")"){
      regex_spe_detect <- function(inpt){
              fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
                ptrn <- grep(ptrn_fill, inpt_v)
                while (length(ptrn) > 0){
                  ptrn <- grep(ptrn_fill, inpt_v)
                  idx <- ptrn[1] 
                  untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
                  pre_val <- inpt_v[(idx - 1)]
                  inpt_v[idx] <- pre_val
                  if (untl > 0){
                    for (i in 1:untl){
                      inpt_v <- append(inpt_v, pre_val, idx)
                    }
                  }
                ptrn <- grep(ptrn_fill, inpt_v)
                }
                return(inpt_v)
              }
         inpt <- unlist(strsplit(x=inpt, split=""))
         may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
         pre_idx <- unique(match(x=inpt, table=may_be_v))
         pre_idx <- pre_idx[!(is.na(pre_idx))]
         for (el in may_be_v[pre_idx]){
                 for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                         inpt <- append(x=inpt, values="\\", after=(i-1))
                 }
         }
         return(paste(inpt, collapse=""))
      }
      lst <- unlist(strsplit(x=inpt, split=""))
      lst_par <- c()
      lst_par_calc <- c()
      lst_pos <- c()
      paires = 1
      pre_paires = 1
      pre_paires2 = 1
      if ((length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2) > 0){
              for (i in 1:(length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2)){ 
                      lst_par <- c(lst_par, 0)
                      lst_par_calc <- c(lst_par_calc, 0)
                      lst_pos <- c(lst_pos, 0)
              }
      }
      vec_ret <- c()
      par_ = 1
      lvl_par = 0
      for (el in 1:length(lst)){
         if (lst[el] == ptrn1){
                 if (!(is.null(vec_ret))){
                         lst_par_calc[pre_paires2:pre_paires][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] + 1
                 }else{
                         lst_par_calc[pre_paires2:pre_paires] <- lst_par_calc[pre_paires2:pre_paires] + 1
                 }
                 pre_paires = pre_paires + 1
                 pre_cls <- TRUE
                 lst_pos[par_] <- el
                 par_ = par_ + 1
                 lvl_par = lvl_par + 1
         }
         if (lst[el] == ptrn2){
                 lvl_par = lvl_par - 1
                 if (!(is.null(vec_ret))){
                      lst_par_calc[c(pre_paires2:pre_paires)][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] - 1
                      pre_val <- lst_par_calc[pre_paires2:pre_paires][vec_ret]
                      lst_par_calc[pre_paires2:pre_paires][vec_ret] <- (-2)
                 }else{
                      lst_par_calc[c(pre_paires2:pre_paires)] <- lst_par_calc[pre_paires2:pre_paires] - 1
                 }
                 if (!(is.null(vec_ret))){ 
                         pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])
                         lst_par_calc[pre_paires2:pre_paires][vec_ret] <- pre_val 
                 }else{
                         pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires])
                 }
                 cnt_par = 1
                 cnt2 = 0
                 if (!(is.null(vec_ret))){
                         vec_ret <- sort(vec_ret)
                         if (pre_mtch[1] >= min(vec_ret)){
                              cnt2 = 2
                              while (pre_mtch[1] > cnt_par & cnt2 <= length(vec_ret)){
                                      if ((vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) > 1){
                                              cnt_par = cnt_par + (vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) - 1
                                      }
                                      cnt2 = cnt2 + 1
                              }
                              if (pre_mtch[1] > cnt_par){
                                      cnt_par = length(vec_ret) / 2 + 1
                              }
                              cnt2 = cnt2 - 1
                         }
                 }
                 lst_par[pre_mtch[1] + (pre_paires2 - 1) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1))] <- paires 
                 lst_par[pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret)] <- paires 
                 if ((pre_mtch[1] + (pre_paires2 - 1)) == 1){
                      pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
                      vec_ret <- c()
                      cnt_par = 0
                 } else if (lst_par_calc[(pre_mtch[1] + (pre_paires2 - 1) - 1)] == -1 & ifelse(is.null(vec_ret), TRUE, 
                              is.na(match(x=-1, table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])))){

                      pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
                      vec_ret <- c()
                      cnt_par = 0
                 } else{
                      vec_ret <- c(vec_ret, (pre_mtch[1]) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1)), 
                                   (pre_mtch[2] + length(vec_ret)))
                 }
                 paires = paires + 1
                 pre_paires = pre_paires + 1
                 pre_cls <- FALSE
                 lst_pos[par_] <- el
                 par_ = par_ + 1
         }
      }
      return(list(lst_par, lst_pos))
      }
      pairs_findr_merger <- function(lst1=list(), lst2=list()){
        better_match <- function(inpt_v=c(), ptrn, untl=1, nvr_here=NA){
          Rtn_v <- c()
          if (length(untl) < length(ptrn)){
            val_add <- untl[length(untl)]
            while (length(untl) < length(ptrn)){
              untl <- c(untl, val_add)
            }
          }
          for (cur_ptrn in 1:length(ptrn)){
            rtn_v <- c()
            cnt = 1
            stop <- FALSE
            while (length(rtn_v) < untl[cur_ptrn] & cnt < (length(inpt_v) + 1) & !(stop)){
                    pre_match <- match(x=ptrn[cur_ptrn], table=inpt_v)
                    if (!(is.na(pre_match))){
                      inpt_v[pre_match] <- nvr_here
                      rtn_v <- c(rtn_v, pre_match)
                    }else{
                      stop <- TRUE
                    }
                    cnt = cnt + 1
            }
            Rtn_v <- c(Rtn_v, rtn_v)
          }
          return(Rtn_v)
        }
        pair1 <- unlist(lst1[1])
        pos1 <- unlist(lst1[2])
        pair2 <- unlist(lst2[1])
        pos2 <- unlist(lst2[2])
        stop <- FALSE
        cnt = 1
        while (!(stop)){
          mtch1 <- match(x = cnt, table = pair1)
          mtch2 <- match(x = cnt, table = pair2)
          if (all(!(is.na(mtch1)), !(is.na(mtch2)))){
            if (pos1[mtch1] < pos2[mtch2]){
              poses <- better_match(inpt_v = pair2, ptrn = c(cnt:max(pair2)), untl = 2)
              pair2[poses] <- pair2[poses] + 1
            }else{
              poses <- better_match(inpt_v = pair1, ptrn = c(cnt:max(pair1)), untl = 2)
              pair1[poses] <- pair1[poses] + 1
            }
          }else{
            stop <- TRUE
          }
          cnt = cnt + 1
        }
        if (length(pair1) > length(pair2)){
          rtn_pos <- pos1
          rtn_pair <- pair1
          add_pos <- pos2
          add_pair <- pair2
        }else{
          rtn_pos <- pos2
          rtn_pair <- pair2
          add_pos <- pos1
          add_pair <- pair1
        }
        cnt = 1
        stop <- FALSE
        pre_lngth <- length(rtn_pos)
        while (cnt <= (pre_lngth / 2 + length(add_pair) / 2) & !(stop)){
          if (is.na(match(x = cnt, table = rtn_pair))){
              cur_add_pos_id <- grep(x = add_pair, pattern = cnt)
              if (cnt < max(rtn_pair)){
                incr = 1
                cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
                while (identical(integer(0), cur_grep)){
                    incr = incr + 1
                    cur_grep <- grep(x = rtn_pair, pattern = (cnt + incr))
                }
                if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[2]] & 
                    rtn_pos[cur_grep[1]] > add_pos[cur_add_pos_id[1]]){
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                  cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[2]])
                  cur_pos <- which.min(cur_vec)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
                }else{
                  if (!(is.na(match(x = (cnt - 1), table = rtn_pair)))){
                        cur_grep2 <- grep(x = rtn_pair, pattern = (cnt - 1))
                        if (rtn_pos[cur_grep2[2]] > add_pos[cur_add_pos_id[2]]){
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep2[1])
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep2[1] + 1))
                          rtn_pos <- append(x = rtn_pos, 
                                            value = add_pos[cur_add_pos_id[1]], after = cur_grep2[1])
                          rtn_pos <- append(x = rtn_pos, 
                                            value = add_pos[cur_add_pos_id[2]], after = (cur_grep2[1] + 1))
                        }else{
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                          rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                          rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                          rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
                        }
                  }else{
                    rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                    rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_grep[1] - 1))
                    rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = (cur_grep[1] - 1))
                    rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_grep[1] - 1))
                  }
                }
              }else{
                incr = 1
                cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
                while (identical(integer(0), cur_grep)){
                  incr = incr + 1
                  cur_grep <- grep(x = rtn_pair, pattern = (cnt - incr))
                }
                if (rtn_pos[cur_grep[2]] < add_pos[cur_add_pos_id[1]]){
                  cur_vec <- abs(rtn_pos - add_pos[cur_add_pos_id[1]])
                  cur_pos <- which.min(cur_vec)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_pos)
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = (cur_pos + 1))
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_pos)
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = (cur_pos + 1))
                }else{
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
                  rtn_pair <- append(x = rtn_pair, value = cnt, after = cur_grep[1])
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[1]], after = cur_grep[1])
                  rtn_pos <- append(x = rtn_pos, value = add_pos[cur_add_pos_id[2]], after = cur_grep[1])
                }
              }
          }
          cnt = cnt + 1
        }
        return(list(rtn_pair, sort(rtn_pos)))
      }
    cur_lst <-  pairs_findr(inpt = inpt, ptrn1 = corr_v[1], ptrn2 = flagged_pair_v[1])
    if (length(corr_v) > 1){
      for (pair in 2:length(corr_v)){
        cur_fun <- pairs_findr(inpt = inpt, ptrn1 = corr_v[pair], ptrn2 = flagged_pair_v[pair]) 
        cur_lst <- pairs_findr_merger(lst1 = cur_lst, lst2 = cur_fun)
      }
    }
    lst_pair <- unlist(cur_lst[1])
    lst_pos <- unlist(cur_lst[2])
    cur_depth <- depth_pairs_findr(inpt = unlist(cur_lst[1])) 
    par = 2
    while (par <= length(lst_pair)){
      if (lst_pair[(par - 1)] != lst_pair[par] & abs(lst_pos[(par - 1)] - lst_pos[par]) > 1){
        cnt = lst_pos[(par - 1)]
        ahd <- TRUE
        if (!(is.na(match(x = inpt[(lst_pos[(par - 1)] + 1)], table = flagged_conj_v)))){
          if (abs(lst_pos[(par - 1)] - lst_pos[par]) == 2){
            ahd <- FALSE
          }
          else{
            cnt = cnt + 1
          }
        }
        if (ahd){
          inpt <- append(x = inpt, value = method[1], after = cnt)
          cnt = cnt + 2
          stop <- FALSE
          while (!(stop) & cnt <= length(inpt)){
            if (is.na(match(x = inpt[cnt], table = cur_vec))){
              cnt = cnt + 1
            }else{
              stop <- TRUE
            }
          }
          inpt <- append(x = inpt, value = method[2], after = (cnt - 1))
          cur_lst <-  pairs_findr(inpt = inpt, ptrn1 = corr_v[1], ptrn2 = flagged_pair_v[1])
          if (length(corr_v) > 1){
             for (pair in 2:length(corr_v)){
               cur_fun <- pairs_findr(inpt = inpt, ptrn1 = corr_v[pair], ptrn2 = flagged_pair_v[pair]) 
               cur_lst <- pairs_findr_merger(lst1 = cur_lst, lst2 = cur_fun)
             }
          }
          lst_pair <- unlist(cur_lst[1])
          lst_pos <- unlist(cur_lst[2])
          cur_depth <- depth_pairs_findr(inpt = unlist(cur_lst[1]))
        }
      }
      par = par + 1
    }
  }
  return(paste(inpt, collapse = ""))
}

