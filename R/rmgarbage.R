
# rmgarbage
#
# Tries to remove strings obtained from OCR engines which are garbage.
# An implementation of the paper
#
# 'Automatic Removal of “Garbage Strings” in OCR Text: An Implementation'
# - by Kazem Taghva , Tom Nartker , Allen Condit , Julie Borsack
#
# References
# ==========
#   [1] http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.81.8901
#   [2] https://github.com/foodoh/rmgarbage/
#   [3] https://ladyissy.github.io/OCR/


#' Rule L: Too long
#'
#' Tests whether the string passed is more than 20 characters,
#' if yes. It is garbage! From https://github.com/foodoh/rmgarbage
#'
#' @param x a character vector
#' @param length numeric, threshold for acceptable length of string
#'
#' @return Logical
#' @export
#'
#' @examples
#' garbage_string_n39 <- stringi::stri_rand_strings(1, 39, pattern = "[a-z]")
#' garbage_string_n40 <- stringi::stri_rand_strings(1, 40, pattern = "[a-z]")
#'
#' rmg_toolong(garbage_string_n39)  # FALSE
#' rmg_toolong(garbage_string_n40)  # TRUE
#'
rmg_toolong <-
  function(x, length = 20){

    ifelse(stringi::stri_length(x) >= length, TRUE, FALSE)

  }

#' Rule A: alphanumeric ratio
#'
#' If a string's ratio of alphanumeric characters to total
#' characters is less than 50%, the string is garbage.
#' From https://github.com/foodoh/rmgarbage
#'
#' @param x a character vector
#' @param threshold a value between 0 and 1 for the alphanumeric to puncutation ratio
#'
#' @return Logical
#' @export
#'
#' @examples
#'
#' garbage_string_r1 <-
#'   paste0(stringi::stri_rand_strings(1, 2, pattern = "[a-zA-Z0-9]"),
#'         "14.9˜;tv˜;<F~~~~9er&é(er@@@@|^˜:..", # this is garbage
#'           collapse = "")
#'
#' garbage_string_r2 <-
#'   stringi::stri_rand_strings(1, 10, pattern = "[a-zA-Z0-9]")
#'
#'   rmg_bad_alnum_ratio(garbage_string_r1) # TRUE
#'   rmg_bad_alnum_ratio(garbage_string_r2) # FALSE
#'
rmg_bad_alnum_ratio <- function(x, threshold = 0.5){

  len <- stringi::stri_length(x)
  nchar_alphanum <- stringi::stri_count_charclass(x, "[a-zA-Z0-9]")

  ifelse(nchar_alphanum / len <= threshold, TRUE, FALSE)

}

#' Rule R: four identical characters
#'
#' If a string has 4 identical characters in a row, it is garbage.
#' From https://github.com/foodoh/rmgarbage
#'
#' @param x a character vector
#' @param n integer, how identical consequtive characters to detect
#'
#' @return Logical
#' @export
#'
#' @examples
#'
#' garbage_string_4c1 <-  "aaaaazzzezrertrg"
#' garbage_string_4c2 <-  "azertyuiopqsdfghj"
#'
#' rmg_consecutive_four_identical(garbage_string_4c1) # TRUE
#' rmg_consecutive_four_identical(garbage_string_4c2) # FALSE
#'
#'
rmg_consecutive_four_identical <- function(x, n = 4L){

  out1 <- strsplit(x, "")[[1]]
  inds = rle(out1)
  any(inds$lengths >= n)
}

#' Rule V: consonants to vowels ratio
#'
#' If a string has nothing but alphabetic characters, look at the
#' number of consonants and vowels. If the number of one is less than 10%
#' of the number of the other, then the string is garbage.
#' From https://github.com/foodoh/rmgarbage
#'
#' @param x a character vector
#' @param ratio numeric, ratio of vowels to consonents
#'
#' @return Logical
#'
#' @return
#' @export
#'
#' @examples
#'
#'  garbage_string_cv1 <- "azerytugino"
#'  garbage_string_cv2 <- "aaaaaaaaaaaaaaaaaax"
#'
#'  rmg_bad_consonant_vowel_ratio(garbage_string_cv1) # FALSE
#'  rmg_bad_consonant_vowel_ratio(garbage_string_cv2) # TRUE
#'
rmg_bad_consonant_vowel_ratio <- function(x, ratio = 0.1){

  len <- stringi::stri_length(x)
  alpha_count <- stringi::stri_count_charclass(x, "[a-zA-Z]")

 # if (alpha_count / len != 1) stop("String contains non-alphabetic characters")

  vowel_count <-  nchar(gsub("[^aeiouy]","", x, ignore.case = TRUE))
  consonant_count <-  alpha_count - vowel_count

  if(consonant_count >= 0 & vowel_count >= 0){
    vc_ratio = vowel_count / consonant_count
   out <- ifelse(vc_ratio < ratio | vc_ratio > 10, TRUE, FALSE)
  } else {
    out <- ifelse(vowel_count == 0 & consonant_count > nchar('rhythms'), TRUE,
                  ifelse(consonant_count == 0 & vowel_count > nchar('IEEE'), TRUE, FALSE))
  }
  return(out)
}


#' Rule P: punctuation characters in a string
#'
#' Strip off the first and last characters of a string. If there
#' are two distinct punctuation characters in the result, then the string
#' is garbage. Stripping off the last two characters as false positives
#' includes those ending with ').' and similar.
#'
#' @param x a character vector
#' @param last_n numeric, number of final characters to strip
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#'
#' garbage_string_2p1 <- "'hi!cou?cou,"
#' garbage_string_2p2 <-  "jean-marc,"
#'
#' rmg_has_two_distinct_puncts_inside(garbage_string_2p1) # TRUE
#' rmg_has_two_distinct_puncts_inside(garbage_string_2p2) # FALSE
#'
rmg_has_two_distinct_puncts_inside <- function(x, last_n = 2){

  out1 <- gsub(paste0('.{', last_n, '}$'), '', x)

  out2 <- stringi::stri_extract_all_regex(out1, "[[:punct:][:symbol:]]")[[1]]

  out3 <- length(unique(out2))

  ifelse(out3 >= 2, TRUE, FALSE)

}

#' Rule C: internal capitals
#'
#' If a string begins and ends with a lowercase letter, then if
#' the string contains an uppercase letter anywhere in between,
#' then it is removed as garbage.
#'
#' @param x a character vector
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#'
#' garbage_string_u1 <- "stratigraphic"
#' garbage_string_u2 <- "sUatigraphic"
#'
#' rmg_has_uppercase_within_lowercase(garbage_string_u1) # FALSE
#' rmg_has_uppercase_within_lowercase(garbage_string_u2) # TRUE
#'
#'
rmg_has_uppercase_within_lowercase <- function(x){

  first_chr_lower <- grepl("^[[:lower:]]", x)
  last_chr_lower <- grepl("[[:lower:]]$", x)
  x_mid <- substring(x, 2, nchar(x)-1)
  middle_chr_upper <- grepl("[[:upper:]]", x_mid)

  all(c(first_chr_lower, last_chr_lower, middle_chr_upper))

}


#' Upper to lowercase ratio
#'
#' If the number of uppercase characters
#' in a string is greater than the number of lowercase
#' characters, and if the number of uppercase characters
#' is less than the total number of characters in the string,
#' it is garbage. From https://ladyissy.github.io/OCR/
#'
#' @param x a character vector
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#'
#' garbage_string_ul1 <- "UNESCO"
#' garbage_string_ul2 <- "POTAto"
#'
#' rmg_uppercase_lowercase_ratio(garbage_string_ul1) # FALSE
#' rmg_uppercase_lowercase_ratio(garbage_string_ul2) # TRUE
#'
#'
rmg_uppercase_lowercase_ratio <- function(x){

  n_total <- nchar(x)
  n_upper <- stringi::stri_count_charclass(x, "[[:upper:]]")
  n_lower <- stringi::stri_count_charclass(x, "[[:lower:]]")

  all_upper <- n_upper == n_total

  ifelse((n_upper > n_lower) & !all_upper, TRUE, FALSE)

}



#' Apply all tests
#'
#' Applys all the tests to detect any kind of OCR garbage
#'
#' @param x a character vector
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#' rmgarbage("Tptpmn") # TRUE
#' rmgarbage("UNESCO") # FALSE
#' rmgarbage("bmarwick@uw.edu") # TRUE
#'
#'
rmgarbage <- function(x){
  any(
  rmg_bad_alnum_ratio(x),
  rmg_bad_consonant_vowel_ratio(x),
  rmg_consecutive_four_identical(x),
  rmg_has_two_distinct_puncts_inside(x),
  rmg_has_uppercase_within_lowercase(x),
  rmg_toolong(x),
  rmg_uppercase_lowercase_ratio(x)
  )
}

