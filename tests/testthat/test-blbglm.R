test_that("blblm works", {
  fitaaaa <- blbglm(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris,m=10,B=100,parallel = TRUE)
  confaa <- confint.blbglm(fitaaaa,c("Sepal.Width","Petal.Length"),level = 0.95,parm=NULL)
  prdictaa <- predict.blbglm(fitaaaa, data.frame(Sepal.Width=c(3.0,3.9),Petal.Length=c(1.1,2.2)),confidence=FALSE,level=0.95)
  expect_equal(coef.blbglm(fitaaaa), coef(fitaaaa))
  expect_equal(sigma.blblm(fitaaaa),sigma(fitaaaa))
  expect_equivalent(confaa,confint(fitaaaa,c("Sepal.Width","Petal.Length"),level=0.95))
  expect_equivalent(prdictaa,predict(fitaaaa, data.frame(Sepal.Width=c(3.0,3.9),Petal.Length=c(1.1,2.2))))
})
