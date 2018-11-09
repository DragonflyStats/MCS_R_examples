
#### Code Segment 1 : Load R Packages ####
library(MethComp)
library(nlme)
library(lme4)
library(magrittr)
library(tidyr)
library(broom)
library(dplyr)

#### Code Segment 2 : Prepare FAT data ###

# - Load from MethComp R package
# - Response Variable: Viscous Fat
# - Methods: KL and SL
# - items: 43 items (with IDs between 1 and 46)
# - Replicates : Consistently 3 replicate measurements for both methods of measurement.

data(fat)
fat <- fat %>% rename("item" = Id, "repl" = Rep, "meth" = Obs, "y" = Vic) %>% select(-Sub)
fat <- fat %>% mutate(item = factor(item))

#### Code Segment 4 : Basic Model 1 (nlme4) ####

# - This model fits an LME model using lme4
# - fixed effect : meth
# - random effect : item inside method

fat.lme4.basic <- lmer( y ~ meth + (meth|item), data=fat)




# -  Response variables: y
# -  Fixed effect: meth
# -  Random effect : item  
# -  (Also: Error term as random effect) 

Model1 <- (lmer(y ~ meth + (1|item), data = fat) )

Model2 <- (lmer(y ~ meth-1 + (meth|item), data = fat))


fat.roy.1 = lme(y ~ meth-1, data = fat,
                random = list(item=pdDiag(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")



# basic models fitted with nlme
# - Random effect is "replicate with item"
fat.nlme.1 = lme(y ~ meth-1, data = fat,
                random = ~1 | item/repl, 
                method="ML")


#### Code Segment 5: Identity Matrix (nlme) ####

fat.mcs.1 = lme(y ~ meth-1, data = fat,
                random = list(item=pdIdent(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")

#### Code Segment 6: Diagonal Matrix (nlme) ####

# - 
# -

fat.mcs.2 = lme(y ~ meth-1, data = fat,
                random = list(item=pdDiag(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")

#### Code Segment 7: Symmetric Matrix (nlme) ####

fat.mcs.3 = lme(y ~ meth-1, data = fat,
                random = list(item=pdSymm(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")

#### Code Segment 8: Compound Symmetric Matrix ####

fat.mcs.4 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")


#### Code Sgement 9: #### 


fat.mcs.1 = lme(y ~ meth-1, data = fat,
                random = list(item=pdIdent(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")



fat.mcs.2 = lme(y ~ meth-1, data = fat,
                random = list(item=pdDiag(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")



fat.mcs.3 = lme(y ~ meth-1, data = fat,
                random = list(item=pdSymm(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")



fat.mcs.4 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")

#### Code Segment 8: Compound Symmetric Matrix ####

fat.mcs.4 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                #weights=varIdent(form=~1|meth),
                #correlation = corSymm(form=~1 | item/repl), 
                method="ML")


#### Code Sgement 9: #### 

#### Code Segment 8: Compound Symmetric Matrix ####

fat.roy.4 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                weights=varIdent(form=~1|meth),
                correlation = corSymm(form=~1 | item/repl), 
                method="ML")


#### Code Segment 9: Roy's Four Models #### 

# - Reference Model
# - Used as H1 in each of the subsequent tests
# - G: Symmetric (unconstrained)
# - Sigma : Symmetric (unconstrained)

fat.roy.1 = lme(y ~ meth-1, data = fat,
                random = list(item=pdSymm(~ meth-1)), 
                weights=varIdent(form=~1|meth),
                correlation = corSymm(form=~1 | item/repl), 
                method="ML")

# - Used for Test 1
# - G: Compound Symmetric (equality constraint)
# - Sigma : Symmetric (unconstrained)

fat.roy.2 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                weights=varIdent(form=~1|meth),
                correlation = corSymm(form=~1 | item/repl), 
                method="ML")

# - Used for Test 2
# - G: Symmetric (unconstrained)
# - Sigma: Compound Symmetric (equality constraint)

fat.roy.3 = lme(y ~ meth-1, data = fat,
                random = list(item=pdSymm(~ meth-1)), 
                weights=varIdent(form=~1|meth),
                correlation = corCompSymm(form=~1 | item/repl), 
                method="ML")

# - Used for Test 3
# - G:  Compound Symmetric (equality constraint)
# - Sigma: Compound Symmetric (equality constraint)

fat.roy.4 = lme(y ~ meth-1, data = fat,
                random = list(item=pdCompSymm(~ meth-1)), 
                weights=varIdent(form=~1|meth),
                correlation = corCompSymm(form=~1 | item/repl), 
                method="ML")
