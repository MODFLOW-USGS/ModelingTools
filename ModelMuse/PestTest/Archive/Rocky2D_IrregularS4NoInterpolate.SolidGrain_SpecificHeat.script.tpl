ptf $
#Script for PLPROC

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat.PstValues')

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
  file='arrays\Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4NoInterpolate.SolidGrain_SpecificHeat_3.arrays';delim="space", &
  plist=p_Value3)
