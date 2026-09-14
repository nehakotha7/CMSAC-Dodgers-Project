library(tidyverse)
theme_set(theme_light())
flying_etiquette <- fly |> 
  filter(!is.na(do_you_recline), !is.na(rude_to_recline))


table(flying_etiquette$do_you_recline)

flying_etiquette |> 
  count(do_you_recline)

flying_etiquette |>
  group_by(do_you_recline) |>
  summarize(freq=n())
  

prop.table(table(flying_etiquette$do_you_recline))

flying_etiquette |> 
  select(do_you_recline) |> 
  table() |> 
  prop.table()

flying_etiquette |> 
  count(do_you_recline) |> 
  mutate(prop=n/sum(n))


flying_etiquette |> 
  ggplot(aes(x=do_you_recline))+
  geom_bar()

flying_etiquette |> 
  count(do_you_recline) |> 
  ggplot(aes(x=do_you_recline, y=n))+
  geom_col() +
  coord_flip() # Flips orientation of axis

flying_etiquette |> 
  count(do_you_recline) |> 
  mutate(prop=n/sum(n)) |> 
  ggplot(aes(x=prop, y=do_you_recline))+
  geom_col()+
  geom_label(aes(label=n))


flying_etiquette |> 
  count(do_you_recline) |> 
  mutate(
    prop=n/sum(n),
    se=sqrt(prop*(1-prop)/sum(n)),
    lower=prop-2*se,
    upper=prop+2*se,
    do_you_recline=fct_reorder(do_you_recline, prop)
  ) |> 
  ggplot(aes(x=prop, y=do_you_recline))+
  geom_col()+
  geom_errorbar(aes(xmin=lower,xmax=upper),
                color='orange',
                width=.5,
                linewidth=1)

chisq.test(table(flying_etiquette$do_you_recline))
# At least one of the proportions differs from others


table('Recline?'=flying_etiquette$do_you_recline,
      'Rude?'=flying_etiquette$rude_to_recline)

xtabs(~do_you_recline+rude_to_recline,
      data=flying_etiquette)


# Conditional distribution with 2D categorical data
flying_etiquette |> 
  count(do_you_recline, rude_to_recline) |> 
  ggplot(aes(x=rude_to_recline, y=n, fill=do_you_recline))+
  geom_col(position = 'dodge')+ # Default position is identity, fill gives proportion, dodge gives side-bt-side plots
  facet_wrap(~rude_to_recline, scales='free')


# Categorical heatmaps
flying_etiquette |> 
  group_by(rude_to_recline, do_you_recline) |> 
  summarize(freq=n(),
            joint=n()/nrow(flying_etiquette)) |> 
  ggplot(aes(x=rude_to_recline, y=do_you_recline))+
  geom_tile(aes(fill=joint), color='white')+
  geom_text(aes(label=scales::percent(joint)))+
  scale_fill_gradient2()+
  coord_equal()


# Mosaic plots - spine chart of spine charts
flying_etiquette |> 
  select(rude_to_recline,do_you_recline) |> 
  table() |> 
  mosaicplot(main='')

# Advanced mosaic plot
flying_etiquette |> 
  ggplot() +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), fill = do_you_recline))

#Color coding mosaic plots with pearson residuals
flying_etiquette |> 
  select(rude_to_recline, do_you_recline) |> 
  table() |> 
  mosaicplot(main = "Relationship between reclining frequency and opinion on rudeness", shade = TRUE)
