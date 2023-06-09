
DROP TABLE IF EXISTS [NHSE_Sandbox_Sys_Improv_EOE].temp.Baseline_OPFU_1920
DROP TABLE IF EXISTS [NHSE_Sandbox_Sys_Improv_EOE].temp.Current_OPFU_2224

/*1920 Baseline for ICB*/

SELECT 
  STP_ICS
--,[Provider_or_IS_CCG_Code]
			,FLOOR(SUM (Activity)) AS Activity
	,[Der_Activity_Month]
	,RIGHT([Der_Activity_Month],2) as [Month]
	 ,FLOOR(sum(a.Activity)/sum(Working_Days_in_Month))*sum(Working_Days_in_Month) as [WD_Baseline_Activity]

INTO [NHSE_Sandbox_Sys_Improv_EOE].temp.Baseline_OPFU_1920
FROM [NHSE_Sandbox_ASF_Pricing].[everyone].[Palantir_A&G_Time_Series] a
left join [NHSE_Sandbox_OP_Transformation].[dbo].[working_day_adjustments] b on b.[Month] = a.Der_Activity_Month
	
WHERE [Der_Appointment_Type] = 'FUp'
	AND [Activity_Type] = 'OPA'
	AND [Der_Activity_Month] between '201904' and  '202003'
	and STP_ICS in ('QH8','QHG','QJG','QM7','QMM','QUE')
					
	group by RIGHT([Der_Activity_Month],2),[Der_Activity_Month],STP_ICS

select * from [NHSE_Sandbox_Sys_Improv_EOE].temp.Baseline_OPFU_1920 order by Der_Activity_Month,STP_ICS

/*Current Activity 22/23 and 23/24 at ICB level*/

  select 
  
  a.Der_Activity_Month
  ,RIGHT(a.Der_Activity_Month,2) as [Month]
  ,a.STP_ICS
  --,Provider_Code_2223
  --,a.Provider_or_IS_CCG_Code
  --,Provider_Type
  ,b.Working_Days_in_Month
  ,FLOOR(sum(a.Activity)/Working_Days_in_Month)*Working_Days_in_Month as [WD_Current_Activity]
  ,FLOOR(sum(a.Activity)) as [Current_Activity]
  
  into [NHSE_Sandbox_Sys_Improv_EOE].temp.Current_OPFU_2224
  from   [NHSE_Sandbox_OP_Transformation].[ref].[ERF_OPA] a
  left join [NHSE_Sandbox_OP_Transformation].[dbo].[working_day_adjustments] b on b.[Month] = a.Der_Activity_Month

where Der_Appointment_Type='FUp'
and Activity_Type='OPA'
and a.STP_ICS in ('QH8','QHG','QJG','QM7','QMM','QUE')
and a.Der_Activity_Month>='202204'


group by 
a.STP_ICS
  
  --,Provider_Code_2223
  --,Provider_Type
  ,a.Der_Activity_Month
  --,a.Provider_or_IS_CCG_Code
  ,b.Working_Days_in_Month
  ,RIGHT(a.Der_Activity_Month,2)

  order by Der_Activity_Month,STP_ICS --,Provider_Type,Provider_Code_2223

/*ICB - Join both together*/

 select a.Der_Activity_Month
 ,a.[Month]
 ,a.STP_ICS
 ,WD_Baseline_Activity
 ,a.WD_Current_Activity
 ,b.Activity as [Baseline_Activity]
,a.[Current_Activity] as [Current_Activity]
,(Current_Activity-b.Activity)/b.Activity as [FUp_Reduction_Measure]

 from [NHSE_Sandbox_Sys_Improv_EOE].temp.Current_OPFU_2224 a
 inner join [NHSE_Sandbox_Sys_Improv_EOE].temp.Baseline_OPFU_1920 b on a.[Month] = b.[Month] and a.STP_ICS = b.STP_ICS

 order by Der_Activity_Month,STP_ICS