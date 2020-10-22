####Get descriptives for both datasets####
m_h = read.csv("Maxwell Huff Scored.csv")
m_b = read.csv("maxwell_buchanan scored.csv")

summary(m_h)
summary(m_b)

##Get mean manually coded
mean(m_h$Manual_Scored, na.rm = T)
mean(m_b$manually_coded, na.rm = T)

##Now get means for each scoring criteria
apply(m_h[ , 8:13], 2, mean)
apply(m_b[ , 8:13], 2, mean)
