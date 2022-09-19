#explore the data set
str(orangeec)

#Summary of the data set
summary(orangeec)

#Which countries has GDP > 15000
orangeec[orangeec$`GDP PC`>=15000,]


#Which countries have a contribution to their GDP from tech industries of less than 2%
orangeec[orangeec$`Creat Ind % GDP`<=2,]


#Which countries have Internet penetration > 80% and
#investment in education as a % of GDP > 4.5%
neworangeec <- subset(orangeec, orangeec$`Internet penetration % population` > 80
                      & orangeec$`Education invest % GDP` >= 4.5)

neworangeec

#Which % of the creative industry (tech) will contribute to GDP in countries 
#where Internet penetration is greater than 80% and have an investment in 
#education of more than 4.5%
neworangeectech <- subset(orangeec, orangeec$`Internet penetration % population` > 80
                      & orangeec$`Education invest % GDP` >= 4.5,
                      select = `Creat Ind % GDP`) 

neworangeectech

#the country 1, 7, 15 has an internet penetration of more than 80% with an 
#investment in education of more than 4.5% with respect to its GDP
#they contribute to their GDP from the tech industry on ____

#Rename a variable 
rename(orangeec,c('Creat Ind % GDP'='creative industry (tech)'))


#EDA Scatter plot 
plot(orangeec$Unemployment ~ orangeec$`Education invest % GDP`,
     xlab='Education Investment (%GDP)',
     ylab='Unemployment',
     main='Investment in Education and Unemployment')


#EDA Scatter plot 
#contribution to the creative industires and GDP per capita
plot(orangeec$`GDP PC` ~ orangeec$`Creat Ind % GDP`,
     xlab='Contribution of creative industries to GDP',
     ylab='GDP per capita',
     main='Relationship orange economy and GDP per capita')


#histogram
ggplot()+geom_histogram(data = orangeec,
                        aes(x=`GDP PC`),fill='steelblue1',color='violetred1',
                        binwidth = 2000)+
  labs(x='GDP per capita', y='Number of countries',
       title='GDP per capita in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#distribution of creative industries to GDP
ggplot()+geom_histogram(data = orangeec,
                        aes(x=`Creat Ind % GDP`),fill='steelblue1',color='violetred1',
                        binwidth = 1)+
  labs(x='contribution creative industries to GDP', y='Number of countries',
       title='contribution creative industries to GDP in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# internet penetration in Latin American
ggplot()+geom_histogram(data = orangeec,
                        aes(x=`Internet penetration % population`),fill='steelblue1',color='violetred1',
                        binwidth = 5)+
  labs(x='Internet penetration (%) population', y='Number of countries',
       title='Internet penetration in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#clasifficacion de paises segun el PIB 
economy <- mean(orangeec$`GDP PC`)

economy

#Create a new variable with a new lable (for our box-plot)
orangeec <- orangeec %>%
  mutate(Strong_economy = ifelse(`GDP PC` < economy,
                                 'below average GDP per capita',
                                 'above average GDP per capita'))

#
ggplot(orangeec, aes(x=Strong_economy, y=`Creat Ind % GDP`,
                     fill=Strong_economy))+
  geom_boxplot(aplha=0.4)+
  labs(x='Country Type', y='Creative industires contribution to GDP',
       title='Creative industries contribution to GDP in Latin American countries with high and low GDP per capita')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#internet penetration in these two countries
ggplot(orangeec, aes(x=Strong_economy, y=`Internet penetration % population`,
                     fill=Strong_economy))+
  geom_boxplot(aplha=0.4)+
  labs(x='Country Type', y='Internet penetration(%)',
       title='Internet penetration in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# los rojos (paises sobre la media del GDP) tiene datos mas omogenios 
# el azul riene mucha dispersion muy pobres y muy ricos
# 75% de esos paises tiene una penetracion de internet por encima del 70%



#
ggplot(orangeec,aes(orangeec$`Internet penetration % population`,orangeec$`Creat Ind % GDP`))+
  geom_point(aes(color=factor(Strong_economy), size=orangeec$`GDP Growth %`))+
  labs(x='internet penetration',y='contribution creative industries to GDP',
       title='Internet and creative industries contribution according to economy and growth')


#el tamano de la burbuja es el creciemiento que ha tenido el pais en su PIB en su ultimo ano 


#
my_graph <- ggplot(orangeec,ase(orangeec$`Internet penetration % population`,orangeec$`Creat Ind % GDP`, label=row.names(orangeec)))+
  geom_point()+
  labs(x='internet penetration', y='contribution creative industries to GDP',
       title = 'Internet and creative industries contribution according to economy and growth')


p = ggplotly(my_graph)
p

#Service % and GDP, podria existir una correlacion positiva = los que mas aportan a servicios son los que tienen un mayor pib per captia 
pairs(orangeec[,2:6])

#nos damos cuenta que parece haber una relacion entre la median age y el gdp  
pairs(orangeec[,2:11])

# verificamos la relación que existe entre el GDP.PC y la media de la edad 
# para validar si la visualizacion es correcta
cor(orangeec[,c(2,11)])

#graficamos esta correlaccion
newdata <- subset(orangeec, select = c(2,11))
pairs(newdata)

#Graficamos todos los paises  
#the age structure of a country impact the GDP of a country 
ggplot(orangeec, aes(x= orangeec$`GDP PC`, y= orangeec$`Median age`, label=Country))+
  geom_point() +
  geom_smooth()+
  geom_text()+
  labs(x='PIB Per Cápita', y='Edad media',
       title='Relación entre PIB per cápita y edad media por países')
  theme(legend.position = "none")


#
cor(orangeec[,2:6],use = 'complete.obs')

#check the standard deviation of the creaative industries
desv <- sd(orangeec$`Creat Ind % GDP`,na.rm = TRUE)

#check the standard deviation of the creaative industries
#En promedio en paises de latino america las industrias creativas estan aportando el 3.29
prom <- mean(orangeec$`Creat Ind % GDP`,na.rm = TRUE)

# Hay una desiguldad enorme entre la participacion de las industrias creativas al PIB
# los datos estan muy desviados del promedio 
coefVar <- (desv/prom)*100
coefVar

#create a new variable with a new tag
orangeec <- orangeec %>%
  mutate(Grow_GDP = ifelse(orangeec$`GDP Growth %`>= 2.5,
                           '2,5% or more', 'less than 2.5%'))

orangeec <- orangeec %>%
  mutate(pink=ifelse(orangeec$`Creat Ind % GDP` >= 2.5,
                       'pinker', 'less pink'))


#ranking de los paises que mas aportan a la economica desde las industrias creativas 
orangeec %>%
  arrange(desc(orangeec$`Creat Ind % GDP`))


TopcreativeInd <- orangeec %>%
  filter(Country %in% c('Mexico', 'Panama', 'Argentina', 'Colombia', 'Brazil', 'Paraguay'))

TopcreativeInd
#
TopcreativeInd %>%
  arrange(desc(orangeec$`Creat Ind % GDP`))


ggplot(TopcreativeInd, aes(x=orangeec$`Internet penetration % population`,
                           y=orangeec$`Services % GDP`, size=orangeec$`GDP PC`))+
  geom_point()+
  facet_wrap(~orangeec$Country)




