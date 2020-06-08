test_that("blblm works", {
  fit444 <- blblm(formula=Sepal.Length~Sepal.Width+Petal.Length,data=iris,m=10,B=100,parallel=TRUE)
  conf44 <- confint.blblm(fit444,c("Sepal.Width","Petal.Length"),level = 0.95,parm=NULL)
  prdict44 <- predict.blblm(fit444, data.frame(Sepal.Width=c(3.0,3.9),Petal.Length=c(1.1,2.2)),confidence=FALSE,level=0.95)
  expect_equal(coef.blblm(fit444), coef(fit444))
  expect_equal(sigma.blblm(fit444),sigma(fit444))
  expect_equivalent(conf44,confint(fit444,c("Sepal.Width","Petal.Length")))
  expect_equivalent(prdict44,predict(fit444, data.frame(Sepal.Width=c(3.0,3.9),Petal.Length=c(1.1,2.2))))
})
