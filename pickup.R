require(tidyverse)
require(openxlsx)



# 去年のデータ数を確認 ----------------------------------------------------
# directory -----------------------------------------------------
dir_input = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2021"
dir_out = dir_input
setwd(dir = dir_input)
last = read.xlsx("202010キチジ耳石選定2.xlsx")

last_check = last %>% filter(priority == 1) %>% group_by(length_class) %>% summarize(count = n())
last_check2 = last %>% filter(pickup == 1) %>% group_by(length_class) %>% summarize(count = n())

# 今年 ---------------------------------------------------------
# directory -----------------------------------------------------
dir_input = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022"
dir_input = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2023"
dir_out = dir_input
setwd(dir = dir_input)


# data ----------------------------------------------------------
df = read.csv("202210_1-3leg耳石一覧.csv", fileEncoding = "CP932")
colnames(df)
df2 = df %>% mutate(class = as.numeric(体長.cm.%/%1), id = rep(1:nrow(df)))

# 使えないサンプルを抜く
# remove = unique(df2$備考)
# remove = remove[-1]
# df3 = df2 %>% filter(備考 != "耳石なし")
# df3 = df3 %>% filter(備考 != "使用不可（採取不備）") 
df3 = df2 %>% mutate(tag = ifelse((df2$備考) == "", 1, 0)) %>% filter(tag != 0, BOX.No > 0)
unique(df3$備考)
summary(df3)


check = df3 %>% group_by(class) %>% summarize(count = n())

# # 関数
# pick = function(data){
#   set.seed(0)
#   all =c()
#   
#   len_cate = unique(data$class)
#   for(i in 1:length(len_cate)){
#     p = data %>% filter(class == len_cate[i])
#     
#     if(len_cate[i] < 18){ # 17cm台までは20個体
#       
#       if(nrow(p) < 21){ #サンプル数が20未満の時
#         p3 = p %>% mutate(pickup = 1)
#       }else{
#         p2 = p %>% sample_n(size = 20) %>% mutate(pickup = 1)
#         p3 = left_join(p, p2 %>% select(id, pickup), by = "id")
#       }
#     }else{ # 18cm台以上
#       if(nrow(p) < 15){ #サンプル数が20未満の時
#         p3 = p %>% mutate(pickup = 1)
#       }else{
#         p2 = p %>% sample_n(size = 15) %>% mutate(pickup = 1)
#         p3 = left_join(p, p2 %>% select(id, pickup), by = "id")
#       }
#     }
#     
#     all = rbind(all, p3)
#   }
# }
# 
# all = pick(data = df3)
data = df3
all = c()
len_cate = unique(data$class)
for(i in 1:length(len_cate)){
  p = data %>% filter(class == len_cate[i])
  
  if(len_cate[i] < 18){ # 17cm台までは20個体
    
    if(nrow(p) < 21){ #サンプル数が20未満の時
      p3 = p %>% mutate(pickup = 1)
    }else{
      p2 = p %>% sample_n(size = 20) %>% mutate(pickup = 1)
      p3 = left_join(p, p2 %>% select(id, pickup), by = "id")
    }
  }else{ # 18cm台以上
    if(nrow(p) < 15){ #サンプル数が20未満の時
      p3 = p %>% mutate(pickup = 1)
    }else{
      p2 = p %>% sample_n(size = 15) %>% mutate(pickup = 1)
      p3 = left_join(p, p2 %>% select(id, pickup), by = "id")
    }
  }
  
  all = rbind(all, p3)
}

re_check = all %>% filter(pickup == 1) %>% group_by(class) %>% summarize(count = n())
all = all %>% mutate(若齢魚 = ifelse(class < 10, 1, NA)) %>% arrange(id)
unique(all$備考) #ここでは耳石のない個体が選ばれていることは確認できない

setwd(dir_out)
write.csv(all, "202210 1-3legキチジ耳石一覧yk.csv", na = "", row.names=FALSE, fileEncoding = "CP932")
