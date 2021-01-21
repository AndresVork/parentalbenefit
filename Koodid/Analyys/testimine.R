test <- data.frame(idpar = c(1,1,2,3,3), idch = c(1,2,3,1,4))

test %>%  group_by(idch) %>% 
  mutate(idparmin = min(idpar)) %>% 
  group_by(idpar) %>% 
  mutate(idfamily = min(idparmin)) %>% 
  group_by(idfamily) %>% 
  mutate(nchild = n_distinct(idch),
         nparents = n_distinct(idpar))
