library("palmerpenguins")

t.test(data = penguins, body_mass_g ~ sex)
