# r-package-healthier-u

---
title: "README"

author: "Julia Mullokandova, Isaac Zhao, Carol Shum"

date: "November 15, 2017"

---
Outline of R Package

Package Name:HealthierU

Create Function called Health Analysis
In this function, we will take health variables such as Height and Weight to  establish a diagnosis of their BMI. We will write an equation for BMI. No web scraping will be needed.

Create Function called PopGraph
This function will graph your statistics in comparison to the rest of the U.S. population and print out the diagnosis of underweight, normal, or obese. We will use ggplot for graphing. Stratified data for BMI based on gender and adult age (20+) was retrieved from:
https://www.cdc.gov/nchs/data/series/sr_03/sr03_039.pdf

Create Function called Personal Trainer
For this function, we will develop a personalized exercise regimen based off of your current weight, your target weight, and the period from now until the target date. We will incorporate the intensity of the exercise and the number of hours desired to exercise weekly. This will in turn ouput a list of activities. 
We are planning on webscraping data from: http://www.nutristrategy.com/caloriesburned.htm

Each member of the group will write one of the three functions and assist the other group members in editing the others.

---
## Summary of HealtierU
### How Does This Work
HealthierU is a reference guide to weight loss. It recommends possible activities for users to try given their gender, weight, target weight and commitment. The activies are pulled from http://www.nutristrategy.com/caloriesburned.htm, a website that lists activities and calories burned by weight for each exercise. After uers enter their gender, height, weight, goal weight and commitment, HealtierU will determine whether they should enter a weight loss program based on their BMI. If they are qualified for a weight loss program then suggest the right exercises for them. 

### To Use
User will select gender, height, weight, desired weight, commitment. The program will use those data points to anaylze whether the user is fit for a weight loss program using BMI formula. If the user is underweight, HealthierU will allow the user to know a weight loss program is not recommended. If the user is not underweight, then HealthierU will move on the further analysis. 

User's desired weight and commitment are then analyzed if s/he is not in the underweight category, defined by BMI. If the user is listing unrealistic goals such as losing too many lbs/kg over a short period of time, HealthierU will determine that it is an unsafe plan and no activities fit that the criteria. Likewise, if the user has a realistic approach to weight loss, Healthier will suggest programs for user to try that will help the user to reach his/her weight goal. 

### Types of User Options
Users will enter the following data points: <br />
  -Gender <br />
  -Height <br />
  -Weight <br />
  -Target Weight <br />
  -Commitment 

