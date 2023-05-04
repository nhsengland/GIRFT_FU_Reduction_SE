/*Get Follow Ups from April 2019 to date (excluding March 20202)*/

SELECT
CONVERT(varchar,dateadd(month,datediff(month,0,OPA.[Appointment_Date]),0),103) as [Activity_Month_Year]
,CASE WHEN OPA.[Der_Financial_Year]='2019/20' THEN 'Y' ELSE 'N' END AS [Is_Baseline]
,case when OPA.Der_Provider_Code in ('RC1') then 'RC9'
		when OPA.Der_Provider_Code in ('RQ8','RDD') then 'RAJ'
		else OPA.Der_Provider_Code
	end as [Provider_Org_Code_Mapped]
,f.STP_Code as [ICS_Code]
,f.STP_Name as [ICS_Name]
,case when OPA.Der_Provider_Code in ('RC1') then 'BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST'
when OPA.Der_Provider_Code in ('RQ8','RDD') then 'MID AND SOUTH ESSEX NHS FOUNDATION TRUST' else f.Organisation_Name end as Provider_Name_Mapped
,OPA.[Treatment_Function_Code] as [TFC]
,CASE WHEN OPA.[Treatment_Function_Code] NOT IN ('812', '501','560', '700', '710', '711', '712', '713', '715', '720', '721', '722', '723', '724', '725', '726', '727', '199', '499')	THEN 'Y' ELSE 'N' END AS [TFC_Scope]
,CASE WHEN OPA.[Attendance_Status] in ('5','6') and OPA.[First_Attendance] in ('1','2') THEN 'F2F' ELSE 'NF2F' END AS [Appointment_Type]
,CASE WHEN OPA.[Attendance_Status] in ('5','6') and OPA.[First_Attendance] in ('1','3') THEN 'First' ELSE 'Follow_Up' END AS [Attendance_Type]
,CASE WHEN OPA.[Der_Number_Procedure] > 0 THEN 'Procedure' ELSE 'No_Procedure' END AS [Procedure]
,CASE WHEN Cost.[HRG_Code_Other] LIKE 'WF%' THEN 'Y' ELSE 'N'END AS [WF_Flag]
,count(OPA.[OPA_Ident]) as [Appointments]

--INTO [NHSE_Sandbox_Sys_Improv_EOE].dbo.eoe_follow_up_tracking
FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_OPA] AS OPA
LEFT JOIN [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_OPA_2223_Cost] AS Cost				
	ON OPA.OPA_Ident = Cost.OPA_Ident AND OPA.Der_Financial_Year = Cost.Der_Financial_Year
	LEFT JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ZZZ_TreatmentFunction] d WITH (NOLOCK) on d.Treatment_Function_Code = OPA.Treatment_Function_Code
left join   [NHSE_Reference].[dbo].[tbl_Ref_ODS_Provider_Hierarchies] f

ON

f.Organisation_Code =

--the case statement strips the trailing 00 some providers put on codes which

--does not exist in the lookup table and uses a 3 char match to find STP

--Acute Providers (starts with R) also use a 3 char match

--otherwise a 5 char match is used

CASE
WHEN OPA.Der_Provider_Code='NX500' then 'NX5'
WHEN RIGHT(OPA.Der_Provider_Code,2)  = '00' THEN LEFT(OPA.Der_Provider_Code,3)

WHEN LEFT(OPA.Der_Provider_Code,1)  = 'R' THEN LEFT(OPA.Der_Provider_Code,3)

ELSE OPA.Der_Provider_Code

END


WHERE 
OPA.[Administrative_Category] = '01'	-- NHS Funded
and OPA.[Attendance_Status] in ('5','6')	-- Attended and was seen
and OPA.[First_Attendance] in ('1','2','3','4')	-- First & Follow up attendances
and OPA.[Treatment_Function_Code] <> '812'
and OPA.Der_Financial_Year in ('2019/20','2020/21','2021/22','2022/23') -- Financial Year
--and Treatment_Function_Code in (select [Treatment_Function_Code] from [NHSE_Sandbox_Sys_Improv_EOE].[dbo].[TFCs_Included])
and OPA.Der_Activity_Month not in ('202003') --Exclude March as counterfactual calculated + added in via union SQL
and Region_Code='Y61'
and Der_Provider_Code like 'R%'

GROUP BY

CONVERT(varchar,dateadd(month,datediff(month,0,appointment_date),0),103),
OPA.Der_Financial_Year
,case when OPA.Der_Provider_Code in ('RC1') then 'RC9'
		when OPA.Der_Provider_Code in ('RQ8','RDD') then 'RAJ'
		else OPA.Der_Provider_Code
	end
,case when OPA.Der_Provider_Code in ('RC1') then 'BEDFORDSHIRE HOSPITALS NHS FOUNDATION TRUST'
when OPA.Der_Provider_Code in ('RQ8','RDD') then 'MID AND SOUTH ESSEX NHS FOUNDATION TRUST' else f.Organisation_Name end,
OPA.Treatment_Function_Code,
OPA.Attendance_Status,
OPA.First_Attendance,
OPA.Der_Number_Procedure,
Cost.[HRG_Code_Other]
,f.STP_Code
,f.STP_Name