
# coding: utf-8

# # Summary Statistics 

# ## Set up data (as if first time)

# In[2]:


#get_ipython().magic(u'pylab inline')
import pandas as pd
import sklearn
import matplotlib.cm as cm


# In[115]:


# read in csv
df = pd.read_csv("~\Documents\ORIE\ 4741\ Big\ Messy\ Data/Project/clean_2015_with_sex.csv")


# In[116]:


# Rename some things
df = df.rename(columns={"education_2003_revision":"education", "detail_age":"age", 
                "place_of_death_and_decedents_status":"pod", "hispanic_originrace_recode":"hispanic"})
df = df.drop("Unnamed: 0", axis=1)


# In[117]:


# make appropriate factor variables
df["education"] = df["education"].astype("category")
df["pod"] = df["pod"].astype("category")
df["race"] = df["race"].astype("category")
df["hispanic"] = df["hispanic"].astype("category")


# In[118]:


# keep only conditions that are popular
keep = [np.nan]
num = df['entity_condition_1'].value_counts()
conditions = df['entity_condition_1'].value_counts().index
for i in range(len(num)):
    if num[i]>=2000:
        keep.append(conditions[i])
        
df = df[df['entity_condition_1'].isin(keep)]
df = df[df['entity_condition_2'].isin(keep)]
df = df[df['entity_condition_3'].isin(keep)]
df = df[df['entity_condition_4'].isin(keep)]


# In[119]:


df.head()


# # Get summary plots

# In[214]:


# Age
df.hist(column='age', bins=49)
plt.xlabel("Age")
plt.ylabel("Count")
plt.title("Age of Death Histogram")
plt.savefig("Age_Distribution.png")
plt.show()


# In[216]:


# Education
Count_Stats = df['education'].value_counts()
Count_Stats.plot.barh()
plt.ylabel("Education")
plt.xlabel("Count")
plt.title("Education Bar Plot")
plt.grid()
plt.savefig("Education_dist.png")
plt.show()


# In[264]:


# Marital Status
MS = df['marital_status'].value_counts()
MS.rename({"S":"Single", "M":"Married", "W":"Widowed", "D":"Divorced"}, inplace=True)
MS.plot.barh()
plt.xlabel("Count")
plt.ylabel("Marital Status")
plt.title("Martial Status Bar Plot")
plt.grid()
plt.savefig("MaritalStatus_dist.png")
plt.show()


# In[265]:


# Race ("Other")
other_race = df.race[~df.race.isin(["White", "Black"])]
other_race.value_counts()[1:-2].plot.barh()
plt.ylabel("Race")
plt.xlabel("Count")
plt.title("Breakdown of \"Other\" Races")
plt.grid()
plt.savefig("races.png")
plt.show()


# In[125]:


# Race (White, Black, Other)
race = df.race.value_counts()
other_sum = race[2:].sum()
race = race[0:3]
race[2] = other_sum
race.rename({"Chineese":"Other"}, inplace=True)
race.plot.pie(autopct='%1.0f%%', colors=["Red", "White", "Green"])
plt.axis("equal")
plt.ylabel("")
plt.title("Race Breakdown")
plt.savefig("Race_other.png")
plt.show()


# In[131]:


# Sex
sex = df.sex.value_counts()
sex.plot.pie(autopct='%1.0f%%')
plt.axis('equal')
plt.ylabel("Sex Breakdown")
plt.show()


# # Make One Figure

# In[135]:


plt.figure(1)
plt.subplot(221)
sex = df.sex.value_counts()
sex.plot.pie(autopct='%1.0f%%')
plt.axis('equal')
plt.ylabel("Sex Breakdown")

plt.subplot(222)
race.plot.pie(autopct='%1.0f%%', colors=["Red", "White", "Green"])
plt.axis("equal")
plt.ylabel("")
plt.title("Race Breakdown")

plt.subplot(223)
other_race.value_counts()[1:-2].plot.barh()
plt.ylabel("Race")
plt.xlabel("Count")
plt.title("Breakdown of \"Other\" Races")
plt.grid()

plt.subplot(224)
MS.plot.barh()
plt.xlabel("Count")
plt.ylabel("Marital Status")
plt.title("Martial Status Bar Plot")
plt.grid()
plt.savefig("pie_chats.png")
plt.show()


# # Medical Data

# In[305]:


top_conds = df.entity_condition_1.value_counts()[0:16]
bar_conds = list(top_conds.index)
values = list(top_conds)
condition_dict = {"I46":"Cardiac Arrest", "J96":"Respiratory Failure", "I50":"Heart Failure", "I25":"Chronic Heart Disease",
                 "I21":"Myocardial Infraction", "G30":"Alzheimer's", "C34":"Lung Cancer", "A41":"Sepsis",
                 "F03":"Dementia", "I64":"Stroke", "R09":"Respiratory Disease", "J18":"Pneumonia",
                  "J44":"Obstructive Pulmonary", "C25":"Pancreatic Cancer", "C50":"Breast Cancer",
                 "C18":"Colon Cancer"}
N = len(bar_conds)
bar_color = cm.rainbow(np.linspace(0,1,len(bar_conds)))
index = np.arange(N)
for i in range(N):
    plt.bar(index[i], values[i], color=bar_color[i], label=condition_dict[bar_conds[i]])
plt.legend(prop={'size': 7})
plt.grid()
plt.xlabel("Condition")
plt.ylabel("Count")
plt.title("Popular Conditions")
plt.savefig("Diseases.png")
plt.show()


# In[288]:


# How many are missing entity_condition
print "Missing", df["entity_condition_2"].isnull().sum(), "Condition 2's"
print "Missing", df["entity_condition_3"].isnull().sum(), "Condition 3's"
print "Missing", df["entity_condition_4"].isnull().sum(), "Condition 4's"


# # Summary Cross Tabs

# In[215]:


# Average age for women, average age for men
women = df[df.sex == "F"]
men = df[df.sex=="M"]
print "Avg age for women", round(women.age.mean(), 2)
print "Avg age for men", round(men.age.mean(), 2)
print "Agv overall is", round(df.age.mean(), 2)


# In[255]:


# By Education
edu = df.groupby("education")["age"].mean()
edu = edu.sort_values()
edu = pd.DataFrame({"Education":list(edu.index), "Age":list(edu.values.round(2))})
edu = edu[["Education", "Age"]]
edu


# In[254]:


print edu.to_latex(index=False)


# In[276]:


# By marital_status
single = df.groupby("marital_status")["age"].mean()
single = single.sort_values()
single = pd.DataFrame({"Marital Status":["Divorced", "Married", "Single", "Widowed"],
                       "Age":list(single.values.round(2))})
single = single[["Marital Status", "Age"]]
single


# In[277]:


print single.to_latex(index=False)


# In[280]:


# Race
r = df.groupby('race')["age"].mean()
r = r.sort_values()
r = pd.DataFrame({"Race":list(r.index), "Age":list(r.values.round(2))})
r = r[["Race", "Age"]]
r


# In[281]:


print r.to_latex(index=False)

