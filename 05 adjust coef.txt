# Adjust edges coeff to maintain mean degree 2011
## will need to figure out how to deal with the adjustment when switching to 2019 inputs
## Calibrate with 2019 since Epi should not have an impact??



small_object_est_2011 <- readRDS("~/CAMP-HET-STI/est/small_object_est_2011.rds")

small_object_est_2011$fit_main.het$coef.form[1] <- small_object_est_2011$fit_main.het$coef.form[1] + 1.7
small_object_est_2011$fit_main.het$coef.form.crude[1] <- small_object_est_2011$fit_main.het$coef.form.crude[1] + 1.7

small_object_est_2011$fit_casl.het$coef.form[1] <- small_object_est_2011$fit_casl.het$coef.form[1] +.4
small_object_est_2011$fit_casl.het$coef.form.crude[1] <- small_object_est_2011$fit_casl.het$coef.form.crude[1] + .4

small_object_est_2011$fit_inst.het$coef.form[1] <- small_object_est_2011$fit_inst.het$coef.form[1] + -log(52) + 1.4
small_object_est_2011$fit_inst.het$coef.form.crude[1] <- small_object_est_2011$fit_inst.het$coef.form.crude[1] + -log(52) + 1.4



##############################################
#Save Data

saveRDS(small_object_est_2011, file = "~/CAMP-HET-STI/est/small_object_est_2011_adj.rds")
saveRDS(small_object_est_2011, file = "~/CAMP-HET-STI/scenarios/Model testing/est/small_object_est_2011_adj.rds")



##### 2019 MODELS

small_object_est_2019 <- readRDS("~/CAMP-HET-STI/est/small_object_est_2019.rds")

small_object_est_2019$fit_main.het$coef.form[1] <- small_object_est_2019$fit_main.het$coef.form[1] + 1.7
small_object_est_2019$fit_main.het$coef.form.crude[1] <- small_object_est_2019$fit_main.het$coef.form.crude[1] + 1.7

small_object_est_2019$fit_casl.het$coef.form[1] <- small_object_est_2019$fit_casl.het$coef.form[1]  + .55
small_object_est_2019$fit_casl.het$coef.form.crude[1] <- small_object_est_2019$fit_casl.het$coef.form.crude[1] + .55

small_object_est_2019$fit_inst.het$coef.form[1] <- small_object_est_2019$fit_inst.het$coef.form[1] + -log(52) + 2.5
small_object_est_2019$fit_inst.het$coef.form.crude[1] <- small_object_est_2019$fit_inst.het$coef.form.crude[1] + -log(52) + 2.5



##############################################
#Save Data

saveRDS(small_object_est_2019, file = "~/CAMP-HET-STI/est/small_object_est_2019_adj.rds")
saveRDS(small_object_est_2019, file = "~/CAMP-HET-STI/scenarios/Model testing/est/small_object_est_2019_adj.rds")
