ptf $
#Script for PLPROC

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='SutraTest1a.c_nod')

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='SutraTest1a.Nodal_Porosity.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='SutraTest1a.Nodal_Porosity.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='SutraTest1a.Nodal_Porosity.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex4;column=8, &
  plist=p_Value4;column=9, &
  file='SutraTest1a.Nodal_Porosity.PstValues')

#Read parameter values
a = $                        a$
# Pilot points are not used with a.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * a
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 1)) = p_Value2 * a
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 1)) = p_Value3 * a
temp4=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     4
  # Setting values for parameter a
    # Substituting parameter values in zones
    p_Value4(select=(s_PIndex4 == 1)) = p_Value4 * a

#Write new data values
write_column_data_file(header='no', &
  file='arrays\SutraTest1a.Nodal_Porosity_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\SutraTest1a.Nodal_Porosity_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\SutraTest1a.Nodal_Porosity_3.arrays';delim="space", &
  plist=p_Value3)
write_column_data_file(header='no', &
  file='arrays\SutraTest1a.Nodal_Porosity_4.arrays';delim="space", &
  plist=p_Value4)
