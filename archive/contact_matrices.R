ggplot(contact_matrix_long, aes(x = other_age_grp, y = age_grp, fill = contact)) + 
  geom_tile(colour = "white") +
  viridis::scale_fill_viridis() +
  theme_minimal()