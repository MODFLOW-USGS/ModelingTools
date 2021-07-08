ptf @
#Script for PLPROC

#Read MODFLOW-2005 grid information file
cl_Discretization = read_mf_grid_specs(file="C:\ModelingTools\ModelMuse\PestTest\ChdTest.gsf")#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='ChdTest.Kx.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='ChdTest.Kx.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='ChdTest.Kx.PstValues')

#Read parameter values

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3

#Write new data values
write_column_data_file(header='no', &
  file='arrays\ChdTest.Kx_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\ChdTest.Kx_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\ChdTest.Kx_3.arrays';delim="space", &
  plist=p_Value3)
