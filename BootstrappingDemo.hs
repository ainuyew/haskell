main = print $ newton 0.0001 g g' df_1y

-- Interest Rates
r_on=0.3876
r_tn=0.3876
r_1w=0.408
r_1m=0.4453
r_2m=0.5373
r_3m=0.658
r_6m=0.947
r_1y=1.27835
r_2y=0.009245

-- Day Count Factors (ACT/360)
dcf_on=1/360
dcf_tn=2/360
dcf_1w=9/360
dcf_1m=33/360
dcf_2m=63/360
dcf_3m=96/360
dcf_6m=187/360
dcf_1y=369/360
dcf_18m=551/360
dcf_2y=733/360

-- Discount Factors
df_on=1/(1+r_on/100*dcf_on)
df_settle=df_on/(1+r_tn/100*(dcf_tn-dcf_on))
df_1w=df_cash r_1w dcf_1w
df_1m=df_cash r_1m dcf_1m
df_2m=df_cash r_2m dcf_2m
df_3m=df_cash r_3m dcf_3m 
df_6m=df_cash r_6m dcf_6m
df_1y=df_cash r_1y dcf_1y

-- log-linear interpolation
interpolate x0 x1 x2 y0 y2 = exp ((x1-x0)/(x2-x0)*((log y2) - (log y0)) + log y0)

df_18m x = interpolate dcf_1y dcf_18m dcf_2y df_1y x

-- re-arranged 2y swap equation
g x = (r_2y/100)*dcf_6m*df_6m + (r_2y/100)*(dcf_1y-dcf_6m)*df_1y + (r_2y/100)*(dcf_18m-dcf_1y)*(df_18m x) + (r_2y/100)*(dcf_2y-dcf_18m)*x - df_settle + x

-- derivative of the above function
g' x = r_2y*(dcf_18m-dcf_1y)*df_1y**(1-(dcf_18m-dcf_1y)/(dcf_2y-dcf_1y))*((dcf_18m-dcf_1y)/(dcf_2y-dcf_1y)-1)+r_2y*(dcf_2y-dcf_18m) + 1

-- calculate discount factor LIBOR rates
df_cash r dcf = df_settle/(1+r/100*(dcf-dcf_tn))

-- newton-raphson
newton epsilon f f' x = let x' = x - ((f x)/(f' x))
                            in if (epsilon > abs (x' - x))
                                  then x'
                                  else newton epsilon f f' x'

