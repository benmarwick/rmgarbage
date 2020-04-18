

test_that("rmg_toolong", {

  garbage_string_n39 <- stringi::stri_rand_strings(1, 5, pattern = "[a-z]")
  garbage_string_n40 <- stringi::stri_rand_strings(1, 40, pattern = "[a-z]")

  expect_false( rmg_toolong(garbage_string_n39) ) # FALSE
  expect_true(  rmg_toolong(garbage_string_n40) ) # TRUE
})

test_that("rmg_bad_alnum_ratio", {

 garbage_string_r1 <-
   paste0(stringi::stri_rand_strings(1, 2, pattern = "[a-zA-Z0-9]"),
         "14.9˜;tv˜;<F~~~~9er&é(er@@@@|^˜:..", # this is garbage
           collapse = "")
 garbage_string_r2 <-
   stringi::stri_rand_strings(1, 10, pattern = "[a-zA-Z0-9]")

  expect_false(  rmg_bad_alnum_ratio(garbage_string_r2) ) # FALSE
  expect_true(  rmg_bad_alnum_ratio(garbage_string_r1)  ) # TRUE
})

test_that("rmg_consecutive_four_identical", {

  garbage_string_4c1 <-  "aaaaazzzezrertrg"
  garbage_string_4c2 <-  "azertyuiopqsdfghj"

  expect_false( rmg_consecutive_four_identical(garbage_string_4c2)  )
  expect_true(  rmg_consecutive_four_identical(garbage_string_4c1)  )

})

test_that("rmg_bad_consonant_vowel_ratio", {

  garbage_string_cv1 <- "azerytugino"
  garbage_string_cv2 <- "aaaaaaaaaaaaaaaaaax"
  garbage_string_cv3 <- "CslwWkrm"
  garbage_string_cv4 <- "Tptpmn"
  garbage_string_cv5 <- "Thlrlnd"
  garbage_string_cv6 <- "Thailand"

  expect_false( rmg_bad_consonant_vowel_ratio(garbage_string_cv6)  )
  expect_false( rmg_bad_consonant_vowel_ratio(garbage_string_cv1)  )
  expect_true(  rmg_bad_consonant_vowel_ratio(garbage_string_cv2)  )
  expect_true(  rmg_bad_consonant_vowel_ratio(garbage_string_cv3)  )
  expect_true(  rmg_bad_consonant_vowel_ratio(garbage_string_cv4)  )
  expect_true(  rmg_bad_consonant_vowel_ratio(garbage_string_cv5)  )

})

test_that("rmg_has_two_distinct_puncts_inside", {

 garbage_string_2p1 <-  "'hi!cou?cou,"
 garbage_string_2p2 <-  "jean-marc,"
 garbage_string_2p3 <-   "btkvdy@us1s<F9>8"
 garbage_string_2p4 <-   "w.a.e˜tcet)˜oe˜"
 garbage_string_2p5 <-   "<F9><F9>iA,1llfllwl˜flII˜N"

 expect_false( rmg_has_two_distinct_puncts_inside(garbage_string_2p2) )
 expect_true(  rmg_has_two_distinct_puncts_inside(garbage_string_2p1) )
 expect_true(  rmg_has_two_distinct_puncts_inside(garbage_string_2p3) )
 expect_true(  rmg_has_two_distinct_puncts_inside(garbage_string_2p4) )
 expect_true(  rmg_has_two_distinct_puncts_inside(garbage_string_2p5) )

})

test_that("rmg_has_uppercase_within_lowercase", {

 garbage_string_u1 <- "stratigraphic"
 garbage_string_u2 <- "sUatigraphic"
 garbage_string_u3 <- "aepauWetectronic"
 garbage_string_u4 <- "bAa"

 expect_false(  rmg_has_uppercase_within_lowercase(garbage_string_u1) )
 expect_true(   rmg_has_uppercase_within_lowercase(garbage_string_u2) )
 expect_true(   rmg_has_uppercase_within_lowercase(garbage_string_u3) )
 expect_true(   rmg_has_uppercase_within_lowercase(garbage_string_u4) )

})

test_that("rmg_uppercase_lowercase_ratio", {

 garbage_string_ul1 <- "UNESCO"
 garbage_string_ul2 <- "POTAto"

 expect_false( rmg_uppercase_lowercase_ratio(garbage_string_ul1) )
 expect_true(  rmg_uppercase_lowercase_ratio(garbage_string_ul2) )


})

test_that("rmgarbage", {

  garbage_string_ul1 <- "UNESCO"
  garbage_string_ul2 <- "POTAto"

  expect_false( rmgarbage(garbage_string_ul1) )
  expect_true(  rmgarbage(garbage_string_ul2) )


})
