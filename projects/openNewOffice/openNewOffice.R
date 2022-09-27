#
LATAM[1:7]

#explore the data set
str(LATAM)

#Summary of the data set
summary(LATAM)

#Which countries has GDP per capita > 15000
LATAM[LATAM$`GDP PC`>=15000,]


#Which countries have a contribution to their GDP from tech industries of less than 2%
LATAM[LATAM$`Creat Ind % GDP`<=2,]


#Which countries have Internet penetration > 80% and
#investment in education as a % of GDP > 4.5%
internertLATAM <- subset(LATAM, LATAM$`Internet penetration % population` > 70
                      & LATAM$`Education invest % GDP` >= 4.5)

internertLATAM

#Which % of the creative industry (tech) will contribute to GDP in countries 
#where Internet penetration is greater than 80% and have an investment in 
#education of more than 4.5%
internetLATAMtech <- subset(LATAM, `Internet penetration % population` > 80
                      & `Education invest % GDP` >= 3.5,
                      select = `Creat Ind % GDP`) 

internetLATAMtech

#the country 1, 7, 15 has an internet penetration of more than 80% with an 
#investment in education of more than 4.5% with respect to its GDP
#they contribute to their GDP from the tech industry on ____

#Rename a variable 
rename(LATAM,c('Creat Ind % GDP'='creative industry (tech)'))


#EDA Scatter plot 
plot(LATAM$Unemployment ~ LATAM$`Education invest % GDP`,
     xlab='Education Investment (%GDP)',
     ylab='Unemployment',
     main='Investment in Education and Unemployment')


#EDA Scatter plot 
#contribution to the creative industires and GDP per capita
plot(LATAM$`GDP PC` ~ LATAM$`Creat Ind % GDP`,
     xlab='Contribution of creative industries to GDP',
     ylab='GDP per capita',
     main='Relationship orange economy and GDP per capita')


#histogram
ggplot()+geom_histogram(data = LATAM,
                        aes(x=`GDP PC`),fill='steelblue1',color='tomato',
                        binwidth = 2000)+
  labs(x='GDP per capita', y='Number of countries',
       title='GDP per capita in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#distribution of creative industries to GDP
ggplot()+geom_histogram(data = LATAM,
                        aes(x=`Creat Ind % GDP`),fill='steelblue1',color='violetred1',
                        binwidth = 1)+
  labs(x='contribution creative industries to GDP', y='Number of countries',
       title='contribution creative industries to GDP in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# internet penetration in Latin American
ggplot()+geom_histogram(data = LATAM,
                        aes(x=`Internet penetration % population`),fill='steelblue1',color='violetred1',
                        binwidth = 5)+
  labs(x='Internet penetration (%) population', y='Number of countries',
       title='Internet penetration in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#clasifficacion de paises segun el PIB 
economy <- mean(LATAM$`GDP PC`)

economy

#Create a new variable with a new lable (for our box-plot)
LATAM <- LATAM %>%
  mutate(Strong_economy = ifelse(`GDP PC` < economy,
                                 'below average GDP per capita',
                                 'above average GDP per capita'))

#
ggplot(LATAM, aes(x=Strong_economy, y=`Creat Ind % GDP`,
                     fill=Strong_economy))+
  geom_boxplot(aplha=0.4)+
  labs(x='Country Type', y='Creative industires contribution to GDP',
       title='Creative industries contribution to GDP in Latin American countries with high and low GDP per capita')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#internet penetration in these two countries
ggplot(LATAM, aes(x=Strong_economy, y=`Internet penetration % population`,
                     fill=Strong_economy))+
  geom_boxplot(aplha=0.4)+
  labs(x='Country Type', y='Internet penetration(%)',
       title='Internet penetration in Latin American countries')+
  theme(legend.position = 'none')+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#
ggplot(LATAM,aes(LATAM$`Internet penetration % population`,LATAM$`Creat Ind % GDP`))+
  geom_point(aes(color=factor(Strong_economy), size=LATAM$`GDP Growth %`))+
  labs(x='internet penetration',y='contribution creative industries to GDP',
       title='Internet and creative industries contribution according to economy and growth')


# the size of the bubble is the growth that the country has had in its GDP in its last year


#
my_graph <- ggplot(LATAM,ase(LATAM$`Internet penetration % population`,LATAM$`Creat Ind % GDP`, label=row.names(LATAM)))+
  geom_point()+
  labs(x='internet penetration', y='contribution creative industries to GDP',
       title = 'Internet and creative industries contribution according to economy and growth')


p = ggplotly(my_graph)
p

#Service % and GDP, there could be a positive correlation = those who contribute the most to services are those with a higher GDP PC
pairs(LATAM[,2:6])

#we realize that there seems to be a relationship between middle age and gdp  
pairs(LATAM[,2:11],
      col = 'violetred1')

# verificamos la relación que existe entre el GDP.PC y la media de la edad 
# para validar si la visualizacion es correcta
cor(LATAM[,c(2,11)])

#graficamos esta correlaccion
newdata <- subset(LATAM, select = c(2,11))
pairs(newdata)

#Graficamos todos los paises  
#the age structure of a country impact the GDP of a country 
ggplot(LATAM, aes(x= LATAM$`GDP PC`, y= LATAM$`Median age`, label=Country))+
  geom_point(colour = "cadetblue1")+
  geom_smooth(colour = "violetred1")+
  geom_text(colour = "blue")+
  labs(x='GDP Per Cápita', y='Age Median',
       title='Relationship between GDP per capita and average age by country')
  theme(legend.position = "none")


#
cor(LATAM[,2:6],use = 'complete.obs')

#check the standard deviation of the creaative industries
desv <- sd(LATAM$`Creat Ind % GDP`,na.rm = TRUE)

#check the standard deviation of the creaative industries
#On average in Latin American countries, the creative industries are contributing 3.29
prom <- mean(LATAM$`Creat Ind % GDP`,na.rm = TRUE)

# There is a huge disparity between the share of creative industries in GDP
# The data is very deviated from the average
coefVar <- (desv/prom)*100
coefVar

# create a new variable with a new tag
LATAM <- LATAM %>%
  mutate(Grow_GDP = ifelse(LATAM$`GDP Growth %`>= 2.5,
                           '2,5% or more', 'less than 2.5%'))

LATAM <- LATAM %>%
  mutate(pink=ifelse(LATAM$`Creat Ind % GDP` >= 2.5,
                       'pinker', 'less pink'))


# ranking de los paises que mas aportan a la economica desde las industrias creativas 
LATAM %>%
  arrange(desc(LATAM$`Creat Ind % GDP`))


TopcreativeInd <- LATAM %>%
  filter(Country %in% c('Mexico', 'Panama', 'Argentina', 'Colombia', 'Brazil', 'Paraguay'))

TopcreativeInd
#
TopcreativeInd %>%
  arrange(desc(LATAM$`Creat Ind % GDP`))


ggplot(TopcreativeInd, aes(x=LATAM$`Internet penetration % population`,
                           y=LATAM$`Services % GDP`, size=LATAM$`GDP PC`))+
  geom_point()+
  facet_wrap(~LATAM$Country)






