
#Loading Defensive Action Stats
df_def_action <- read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\Defensive_Actions_2019_20_ManUnited(2).csv")
#View(df_def_action)

#Loading Shooting Action Stats
df_shooting <- read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\Shooting_2019_20_ManUnited(2).csv")
#View(df_shooting)

#Loading Passing Action Stats
df_passing <-
    read.csv(
        "D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\Passing_2019_20_ManUnited(2).csv"
    )
#View(df_passing)

#Loading Standard Stats
df_Standard <- read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\Standard_Stats_2019_20_ManUnited.csv")


#importing dplyr library
library(dplyr)
library(ggplot2)
library(gridExtra)


##The defenders who have been dribbles past the least amount of times 
# COnditions: 1. Have to be a defender, 2. Have had to play more than 19 matches (atleast half of the season), 
#3. Have not be dribbled past more than twice in a match
def_sel_dribs_past <- select(df_def_action, Player, Dribs_Past, Pos, X90s) %>% 
    filter(df_def_action$Dribs_Past < 76 & (df_def_action$Pos == 'DF'| df_def_action$Pos == 'MF') & df_def_action$X90s > 19) 
## Plot for the above data
ggplot(data = def_sel_dribs_past)+ ggtitle("Dribbled Past") + xlab("Player") + ylab("No. of Times Dribbled Past") + geom_bar(mapping = aes(x=Player, y=Dribs_Past,fill=Pos), stat="identity") + 
    geom_text(aes(x=Player,y=Dribs_Past,label=Dribs_Past), vjust=1.6, color="white", size=3.5)


##Most numbers of Pressures applied by a defender
def_most_succ_prss <- select(df_def_action, Player, Prsrs_Press,Prsrs_Succ,Prsrs_., Pos, X90s) %>% 
    filter((df_def_action$Pos == 'DF'| df_def_action$Pos == 'MF') & df_def_action$X90s > 19)
## Plot for the above data
ggplot(data = def_most_succ_prss) + ggtitle("Successful Press") + xlab("Player") + ylab("No. of Successful Press") + geom_bar(mapping = aes(x=Player, y=Prsrs_Succ,fill=Pos), stat="identity") +
    geom_bar(mapping = aes(x=Player, y=Prsrs_Press,fill=Pos), stat="identity")+ 
    geom_text(aes(x=Player,y=Prsrs_Succ,label=Prsrs_Succ), vjust=1.6, color="white", size=3.5)


#try s
ggplot(data = def_most_succ_prss) + geom_bar(mapping = aes(x=Player,y=Prsrs_Press, fill=Pos, position="stacked"), stat="identity")
ggplot(data = def_most_succ_prss, aes(x=Player,y=Prsrs_Press, fill=Pos)) + geom_bar(position="stack", stat="identity")


##MOst press in the Defensive third
#Conditions: 1. Has to be a defender or a midfielder
#2. Have had to play more than 19 matches (atleast half of the season),
def_mst_prss <- select(df_def_action, Player, Prsrs_Def.3rd, Pos, X90s) %>% 
    filter((df_def_action$Pos == 'DF' | df_def_action$Pos == 'MF') & df_def_action$X90s > 19)
#plot
ggplot(data = def_mst_prss) + ggtitle("Most Presses in Def. 3rd") + xlab("Player") + ylab("No. of Presses") + geom_bar(mapping = aes(x=Player, y=Prsrs_Def.3rd,fill=Pos), stat="identity") + 
    geom_text(aes(x=Player,y=Prsrs_Def.3rd,label=Prsrs_Def.3rd), vjust=1.6, color="white", size=3.5)

## Ratio of Successful Presses to Dribbled past
df_SucPrs_Drib <- select(def_most_succ_prss, Player,Prsrs_Press, Prsrs_Succ)
#Combining the dribbled past column 
df_SucPrs_Drib <- cbind(df_SucPrs_Drib, DribPst = def_sel_dribs_past$Dribs_Past)
df_SucPrs_Drib$Unsucc_Prs <- df_SucPrs_Drib$Prsrs_Press - df_SucPrs_Drib$Prsrs_Succ
df_SucPrs_Drib$Ratio <- df_SucPrs_Drib$DribPst/df_SucPrs_Drib$Prsrs_Succ*100


### Midfield Analysis
#Lets look at the passes done by the players
#COndition:
# 1.Have had to play more than 12 matches (atleast 1/3 of the season),
## 2. The player either has to be FW, MD or DF
df_pass_stat <- select(df_passing, Player,  Pos, X90s, Ast, A.xA, Prog) %>% 
    filter((df_passing$Pos == 'MF' | df_passing$Pos == 'DF' |  df_passing$Pos == 'FW') & df_passing$X90s > 12) 
arrange(df_pass_stat, desc(Ast), desc(Prog))

##Plot for the above data
ggplot(data=df_pass_stat) + geom_point(mapping = aes(x=A.xA,y=Ast, color=Pos, size=Prog)) + scale_shape_manual(values=seq(11,25)) + geom_text(aes(x=A.xA,y=Ast,label=Player)) + 
    ggtitle("Assist Vs Expected Assist") + xlab("Expected Assist (A.xA)") + ylab("No. of Assists") + geom_vline(xintercept = 0, color='red',linetype="dashed", size=1) +
    geom_abline(slope=1,intercept=1, linetype = "dashed",color="blue", size=1)

df_pass_stat_2 <- select(df_passing, Player,  Pos, X90s, Ast, xA, Prog) %>% 
    filter((df_passing$Pos == 'MF' | df_passing$Pos == 'DF' |  df_passing$Pos == 'FW') & df_passing$X90s > 12) 
arrange(df_pass_stat_2, desc(Ast), desc(Prog))

library(ggrepel)

##Plot for the above data
ggplot(data = df_pass_stat_2) + geom_point(mapping = aes(
    x = xA,
    y = Ast,
    color = Pos,
    size = Prog
)) + scale_shape_manual(values = seq(11, 25)) + 
    geom_text(aes(x = xA, y =Ast, label = Ast)) +
    geom_label_repel(aes(x = xA, y =Ast, label = Player),
                     box.padding   = 1.5, 
                     point.padding = 0.5,
                     segment.color = 'grey50') +
    ggtitle("Assist Vs Expected Assist") + 
    xlab("Expected Assist (xA)") + 
    ylab("No. of Assists") + 
    geom_vline(
        xintercept = 0,
        color = 'red',
        linetype = "dashed",
        size = 1
    ) +
    geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "blue",
        size = 1
    )

##Pass Types and Completion Analysis
#For Defenders
df_pass_comp_DF <- dplyr::select(df_passing, Player, Pos, X90s, Short_Cmp.,Medium_Cmp.,Long_Cmp.,Total_Cmp.) %>%
    filter( df_passing$Pos == 'DF' & df_passing$X90s > 19) 
#For Midfielders
df_pass_comp_MF <- dplyr::select(df_passing, Player, Pos, X90s, Short_Cmp.,Medium_Cmp.,Long_Cmp.,Total_Cmp.) %>%
    filter(df_passing$Pos == 'MF' & df_passing$X90s > 12) 
#For Forwards
df_pass_comp_FW <- dplyr::select(df_passing, Player, Pos, X90s, Short_Cmp.,Medium_Cmp.,Long_Cmp.,Total_Cmp.) %>%
    filter(df_passing$Pos == 'FW' & df_passing$X90s > 12) 

arrange(df_pass_comp_DF, desc(Total_Cmp.))
arrange(df_pass_comp_MF, desc(Total_Cmp.))
arrange(df_pass_comp_FW, desc(Total_Cmp.))

#Plot of DF
df_pass_plot_DF <- df_pass_plot_DF <- ggplot(data=df_pass_comp_DF) + ggtitle("Pass Completed Defenders") + xlab("Player") + ylab("Passes Completed %") + 
    geom_bar(mapping = aes(x=Player, y=Short_Cmp., fill="Short Pass"), stat="identity")  + geom_text(aes(x=Player,y=Short_Cmp. +2 ,label=Short_Cmp.), vjust=0, color="black", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Medium_Cmp., fill="Medium Pass",alpha=0.99), stat="identity") + geom_text(aes(x=Player,y=Medium_Cmp.,label=Medium_Cmp.), vjust=1.6, color="white", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Long_Cmp., fill="Long Pass"), stat="identity") + geom_text(aes(x=Player,y=Long_Cmp.,label=Long_Cmp.), vjust=1.6, color="white", size=3.5)

#Plot of MF
df_pass_plot_MF <- ggplot(data=df_pass_comp_MF) + ggtitle("Pass Completed Midfielders") + xlab("Player") + ylab("Passes Completed %") + 
    geom_bar(mapping = aes(x=Player, y=Short_Cmp., fill="Short Pass"), stat="identity")  + geom_text(aes(x=Player,y=Short_Cmp. +2 ,label=Short_Cmp.), vjust=0, color="black", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Medium_Cmp., fill="Medium Pass",alpha=0.99), stat="identity") + geom_text(aes(x=Player,y=Medium_Cmp.,label=Medium_Cmp.), vjust=1.6, color="white", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Long_Cmp., fill="Long Pass"), stat="identity") + geom_text(aes(x=Player,y=Long_Cmp.,label=Long_Cmp.), vjust=1.6, color="white", size=3.5)

#Plot of FW
df_pass_plot_FW <- ggplot(data=df_pass_comp_FW) + ggtitle("Pass Completed Forwards") + xlab("Player") + ylab("Passes Completed %") + 
    geom_bar(mapping = aes(x=Player, y=Short_Cmp., fill="Short Pass"), stat="identity")  + geom_text(aes(x=Player,y=Short_Cmp. +2 ,label=Short_Cmp.), vjust=0, color="black", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Medium_Cmp., fill="Medium Pass",alpha=0.99), stat="identity") + geom_text(aes(x=Player,y=Medium_Cmp.,label=Medium_Cmp.), vjust=1.6, color="white", size=3.5) +
    geom_bar(mapping = aes(x=Player, y=Long_Cmp., fill="Long Pass"), stat="identity") + geom_text(aes(x=Player,y=Long_Cmp.,label=Long_Cmp.), vjust=1.6, color="white", size=3.5)

layout <- rbind(c(1,2),
                c(3,3)) 
grid.arrange(df_pass_plot_DF,df_pass_plot_FW,df_pass_plot_MF, layout_matrix = layout)

df_pass_plot_MF

## Progessive DIstance Vs. Total Pass Completion %
df_PrgDist_PsComp <- select(df_passing, Player, Pos, X90s, Total_Cmp.,Total_PrgDist,KP,Ast) %>%
    filter((df_passing$Pos == 'MF'|df_passing$Pos == 'FW') & df_passing$X90s > 12) 
ggplot(data=df_PrgDist_PsComp) + geom_point(mapping = aes(x=Total_Cmp.,y=Total_PrgDist, shape = Player,size=KP,color=as.factor(Ast) )) + scale_shape_manual(values=seq(11,25)) +
    geom_text(aes(x=Total_Cmp.,y=Total_PrgDist,label=paste("(",Total_PrgDist,",",Ast,")"),hjust=0.4,vjust=-0.8)) + ggtitle("Pass Completed % Vs. Prog Dist.") + xlab("Pass Comp. %") + ylab("Prog. Dist. (Yards)") +
    theme(legend.title = element_text(face = "bold"),legend.text=element_text(size=10),legend.key.height = unit(0.5,"cm")) +  facet_wrap(~ Pos)

### Forward Analysis
#
df_goal_stat <- select(df_shooting, Player,  Pos, X90s, Gls, Sh,SoT,xG,npxG) %>% 
    filter((df_shooting$Pos == 'MF' | df_shooting$Pos == 'FW' | df_shooting$Pos == 'MF/FW') & df_shooting$X90s > 12) 
arrange(df_goal_stat, desc(Gls), desc(SoT))

#Plot for Goals vs expected goals
ggplot(data=df_goal_stat) + geom_point(mapping = aes(x=xG,y=Gls, color=Pos, shape = Player, size=SoT)) + scale_shape_manual(values=seq(11,25)) + geom_text(aes(x=xG,y=Gls,label=Gls),vjust = -1) + 
    ggtitle("Goals Vs Expected Goals") + xlab("Expected Goals (xG)") + ylab("Goals Scored") + geom_vline(xintercept = 0, color='red',linetype="dashed", size=1) +
    geom_abline(slope=1,intercept=0, linetype = "dashed",color="blue", size=1)

### Statistical Analysis
## The new incoming players are:
#1. Edinson Cavani, 2. Amad Traore(Amad Dialo), 3. Facundo Pellistri, 4. Alex Telles, 5. Donny VD Beek
## Now let us read the data for the new players who were signed recently
#Defensive Action
df_new_def <-  read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\New Players\\NewPlayers_DefensiveAction_2019-20.csv")
#Passing
df_new_pass <- read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\New Players\\NewPlayers_Passing_2019-20.csv")
#Shooting
df_new_shoot <- read.csv("D:\\Islington\\3rd Sem\\Data Visualization\\Project\\Data\\New Players\\NewPlayers_Shooting_2019-20.csv")

#Cleaned Passing Data
df_pass_cleaned <- dplyr::select(df_passing,Player, Pos, X90s, Ast, xA, KP, Prog) %>% 
    filter((df_passing$Pos == 'MF' | df_passing$Pos == 'DF' |  df_passing$Pos == 'FW') & df_passing$X90s > 10) %>%
    droplevels

#Random Sample
set.seed(42)
random_sample_pass <- sample(1:17,4) #80/20 rule

#training and test sample
df_pass_training <- df_pass_cleaned[-random_sample_pass,]
df_pass_test <- df_pass_cleaned[random_sample_pass,]

library(nnet)

df_pass_cleaned

#logistic(multinomial) regresion
mdl_pass_logs <- multinom(Pos ~ Ast + xA + Prog + KP, data = df_pass_training)
#linear regression
mdl_pass_lin <- glm(Pos ~ Ast + xA + Prog + KP, data = df_pass_training )

#Filtered Shooting Data
df_shoot_filtered <- select(df_shooting,Player,Pos,X90s,Gls,Sh,SoT,xG) %>% 
    filter((df_passing$Pos == 'MF'|  df_passing$Pos == 'DF' |  df_passing$Pos == 'FW') & df_passing$X90s > 10) %>%
    droplevels

#Random Sample
set.seed(42)
random_sample_shoot <- sample(1:17,4) #80/20 rule
#training and test sample
df_shoot_training <- df_shoot_filtered[-random_sample_shoot,]
df_shoot_test <- df_shoot_filtered[random_sample_shoot,]

#Model Training
mdl_shoot_lin <- lm(Gls ~ Sh + SoT + xG, data = df_shoot_training )

pred_goals <- predict(mdl_shoot_lin,df_shoot_test[c("Sh","SoT","xG")])

pred_goals_new <- predict(mdl_shoot_lin,df_new_shoot[c("Sh","SoT","xG")])

###CLASSIFICATION
library(MASS)
#classification(Linear Discrimant Analysis LDA) 
mdl_pass_lda <- lda(Pos ~ Ast + xA + Prog + KP, data = df_pass_training )
#classification(Quadratic Discrimant Analysis QDA)
mdl_pass_lda <- qda(Pos ~ Ast + xA + Prog + KP, data = df_pass_training )

summary(mdl_pass_lda)

pred <- predict(mdl_pass_logs,df_pass_test[c("Ast","xA","Prog", "KP")])
#LDA Prediction
pred_lda_test <- predict(mdl_pass_lda,df_pass_test[c("Ast","xA","Prog", "KP")])
pred_lda_new <- predict(mdl_pass_lda,df_new_pass[c("Ast","xA","Prog", "KP")])

#KNN Classifier
train.X = cbind(df_pass_training[c("Ast","xA","Prog", "KP")])
test.X = cbind(df_pass_test[c("Ast","xA","Prog", "KP")])
train.Pos = df_pass_training[,c("Pos")]

pred_KNN = knn(train.X,test.X,train.Pos,k=1)

#In order to interpret this result I will first plot box plot the predictors used for the model training i.e Assits(Ast), Expected Assists(xA), Key Passes(KP) and Progressive Passes(Prog), according to the position of the players.
#```{r}
#seperating out the passing stats according to position
df_pass_filtered <-
    dplyr::select(df_passing, Player, Pos, X90s, Ast, xA, KP, Prog) %>%
    filter((
        df_passing$Pos == 'FW' |
            df_passing$Pos == 'MF' |
            df_passing$Pos == 'DF'
    ) & df_passing$X90s > 10
    ) %>% droplevels

#assist box plot
box_plt_Ast <- ggplot(df_pass_filtered) +
    geom_boxplot(aes(x = Pos, y = Ast, fill = Pos))

#Key Passes box plot
box_plt_KP <- ggplot(df_pass_filtered) +
    geom_boxplot(aes(x = Pos, y = KP, fill = Pos))

#progressive passes boxplot
box_plt_Prog <- ggplot(df_pass_filtered) +
    geom_boxplot(aes(x = Pos, y = Prog, fill = Pos))

#Expected Assist passes boxplot
box_plt_xA <- ggplot(df_pass_filtered) +
    geom_boxplot(aes(x = Pos, y = xA, fill = Pos))
#```
#```{r fig.width=10,fig.height=10}
layout <- rbind(c(1, 2),
                c(3, 4))
grid.arrange(box_plt_Ast,
             box_plt_KP,
             box_plt_Prog,
             box_plt_xA,
             layout_matrix = layout)
#```

#adding the pass data of new players to the old pass dataframe
df_pass_new_all <- rbind(df_pass_cleaned,df_new_pass[c("Player","Pos","X90s","Ast","xA","KP","Prog")])
df_shoot_filtered_new <- dplyr::select(df_new_shoot,Player,Pos,X90s,Gls,Sh,SoT,xG) %>% droplevels
df_shoot_new_all <- rbind(df_shoot_filtered,df_shoot_filtered_new[c("Player","Pos","X90s","Gls","Sh","SoT","xG")])
df_pass_shot_new <- merge(x = df_pass_new_all, y = df_shoot_new_all[c("Player","Gls","Sh","SoT","xG")], by = "Player", all = TRUE)

df_pass_new <- rbind(df_pass_cleaned,df_new_pass[c("Player","Pos","X90s","Ast","xA","KP","Prog")])

#removing Jadon Sancho
df_pass_shot_new <- df_pass_shot_new[-c(22),]
#df_pass_shot_new2 <- merge(x = df_pass_shot_new, y = df_new_shoot[c("Player","Gls","Sh","SoT","xG")], by = "Player")

#Removing name and position from the data
input_kmeans <- df_pass_new[,3:7]
#input_kmeans <- df_pass_shot_new[,3:11]

#K-Means clustering
#player_cluster3 <- kmeans(input_kmeans, 3, iter.max = 10)
player_cluster3 <- kmeans(input_kmeans, 3, iter.max = 10)

#finding the optimal number of clusters 
k.max <- 10
total_wss <- c()

for (k in 1:k.max) {
    total_wss <- c(total_wss, kmeans(input_kmeans, k, iter.max = 10)$tot.withinss)
}

plot(1:k.max, total_wss,
     type="b", 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#K-Means clustering with optimum number of clusters
player_cluster4 <-kmeans(input_kmeans, 4, iter.max = 10)
#player_cluster5 <-kmeans(input_kmeans, 5, iter.max = 10)

ggplot(df_pass_new, aes(
    x = Prog,
    y = KP,
    col = as.factor(player_cluster5$cluster),
    size = Ast
)) + geom_point() + geom_label_repel(
                    aes(x = Prog, y = KP, label = Player),
                    box.padding   = 1.5,
                    point.padding = 0.5,
                    segment.color = 'grey50'
                )

ggplot(data = df_pass_new) + geom_point(mapping = aes(
    x = Prog,
    y = KP,
    col = as.factor(player_cluster4$cluster),
    size = Ast
)) + scale_shape_manual(values = seq(11, 25)) + 
    geom_text(aes(x = Prog, y =KP, label = Ast)) +
    geom_label_repel(aes(x = Prog, y =KP, label = Player,col=as.factor(player_cluster4$cluster)),
                     box.padding   = 1.5, 
                     point.padding = 0.5,
                     segment.color = "black") +
    ggtitle("Clusters [Progessive Passes Vs. Key Passes]") + 
    xlab("Progessive Passes (Prog)") + 
    ylab("Key Passes (KP)")

#HC Clustering
distance_cluster <- dist(df_pass_new)
class(distance_cluster)
cluster_hier_model <- hclust(distance_cluster)
plot(cluster_hier_model)
rownames(df_pass_new) <- NULL
library(dendextend)
dend <- as.dendrogram(hclust(dist(df_pass_new[,2:7]),"ave"))
df_pass_new[, 2:7] %>%
    dist() %>%
    hclust() %>%
    as.dendrogram() %>%
    color_branches(k = 5) %>%
    color_labels(k = 5) %>%
    place_labels(paste(df_pass_new$Player)) %>%
    plot(horiz = TRUE)

View(df_pass_new)

#Hypothesis Testing
pass_DF <- df_pass_comp_DF$Total_Cmp.
pass_MF <- df_pass_comp_MF$Total_Cmp.

pass_MF_2 <- pass_MF[pass_MF > 80]

res <- t.test(pass_DF, pass_MF_2, var.equal = TRUE)
res

res2 <- t.test(pass_DF, pass_MF_2, var.equal = FALSE)
res2
