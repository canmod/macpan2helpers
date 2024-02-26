# test_that("visCompartmental() returns a `visNetwork` object", {
#   sir = macpan2::Compartmental(system.file('starter_models', 'sir', package = 'macpan2'))
#   vn = visCompartmental(sir)
# 
#   expect_true(
#     "visNetwork" %in% class(vn)
#   )
# })
