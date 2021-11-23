


hdp$Experience2 <-(hdp$Experience-17.64129)/sqrt(var(hdp$Experience))
hdp$Age2 <-(hdp$Age-50.97)/sqrt(var(hdp$Age))


model <- glmer(ntumors ~ IL6+Age2+Sex+Married + (1 + Experience2 | HID/DID), 
               data = hdp, 
               family = 'poisson', 
               control = glmerControl(optimizer = 'bobyqa'))
summary(model)

model2 <- glmer(ntumors ~ IL6+Age2+Sex+Married + (1 | HID/DID), 
               data = hdp, 
               family = 'poisson', 
               control = glmerControl(optimizer = 'bobyqa'))
summary(model2)

model3 <- glmer(ntumors ~ IL6+Age2+Sex+Married + (1 + Experience2 | DID), 
               data = hdp, 
               family = 'poisson', 
               control = glmerControl(optimizer = 'bobyqa'))
summary(model3)

model4 <- glmer(ntumors ~ IL6+Age2+Sex+Married + (1 | DID), 
                data = hdp, 
                family = 'poisson', 
                control = glmerControl(optimizer = 'bobyqa'))
summary(model4)

medico20 <- hdp %>% 
  filter(DID == 20) %>% 
  select(IL6,Age2,Sex,Married, Experience2, HID, DID)

medico20

medico20 %>% predict(model3,., type = "response")

