
#' easySenti
#'
#' Calculation of sentiment value of documents
#' @param doc vector. documents to read
#' @param positive vector. postive words
#' @param negative vector. negative words
#' @param t numeric. threshold to be positive(and -negative). count of positive - count of negative
#' @param sigmoid logic. use or not of sigmoid function
#' @param t.s numeric. threshold to be positive. sigmoid of (positive - negative)
#' @param t.s2 numeric. threshold to be negative. sigmoid of (positive - negative)
#' @return result vector.
#' @export
#' @examples
#' cntSenti(docs = docs, positive=positive, negative=negative, t=0, sigmoid=FALSE, t.s=0.3, t.s2=0.1)

easySenti <- function(docs, positive, negative, t=0, sigmoid=FALSE, t.s=0.3, t.s2=0.1)
{
  library(RHINO)
  initRhino()

  POS <- lapply(docs, getMorph, "ALL")  	        # 형태소 분석

  POS.vec <- unlist(POS)                        #벡터 타입으로 변환
  pos.matches.num <- match(POS.vec, positive)   #긍정어 벡터 번호
  neg.matches.num <- match(POS.vec, negative)  	#부정어 벡터 번호

  pos.matches <- !is.na(pos.matches.num)        #긍정어는 TRUE, 그 외는 FALSE
  neg.matches <- !is.na(neg.matches.num)        #부정어는 TRUE, 그 외는 FALSE

  pos.sum <- sum(pos.matches)	                  #긍정어 검사결과에서 TRUE 개수 카운트
  neg.sum <- sum(neg.matches) 	                #부정어 검사결과에서 TRUE 개수 카운트

  result <- NULL

  if(sigmoid==FALSE){                            # sigmoid = FALSE 인 경우
    #감정값 계산
    result.sum <- pos.sum - neg.sum
    print(paste("[1] number of positive words", pos.sum))
    print(paste("[2] number of negative words", neg.sum))
    print(paste("value by Frequency:", result.sum))
    if(result.sum > 0){
      if(result.sum > t){
        result <- "POSITIVE"
      }else{
        result <- "NEUTRAL"
      }
    }else if(result.sum == 0){
      result <- "NEUTRAL"
    }else{
      if(result.sum < -t){
        result <- "NEGATIVE"
      }else{
        result <- "NEUTRAL"
      }
    }
  }else{                                        # sigmoid = TRUE 인 경우
    result.sum <- sigmoid(pos.sum - neg.sum)
    print(paste("[1] number of positive words", pos.sum))
    print(paste("[2] number of negative words", neg.sum))
    print(paste("value by Sigmoid function:", result.sum))
    result.sum <- round(result.sum, digits = 3)
    if(result.sum > t.s){
      result <- "POSITIVE"
    }else if(result.sum < t.s2){
      result <- "NEGATIVE"
    }else{
      result <- "NEUTRAL"
    }
  }


  return(result)
}


sigmoid <- function(x){
  1 / (1 + exp(-x))
}
