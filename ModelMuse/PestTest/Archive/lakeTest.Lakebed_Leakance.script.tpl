ptf @
#Script for PLPROC

#Read MODFLOW-2005 grid information file
cl_Discretization = read_mf_grid_specs(file="lakeTest.gsf")
#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex4;column=8, &
  plist=p_Value4;column=9, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex5;column=10, &
  plist=p_Value5;column=11, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex6;column=12, &
  plist=p_Value6;column=13, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex7;column=14, &
  plist=p_Value7;column=15, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex8;column=16, &
  plist=p_Value8;column=17, &
  file='lakeTest.Lakebed_Leakance.PstValues')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex9;column=18, &
  plist=p_Value9;column=19, &
  file='lakeTest.Lakebed_Leakance.PstValues')

#Read parameter values
Leak = @                        Leak@
# Pilot points are not used with Leak.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * Leak
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 1)) = p_Value2 * Leak
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 1)) = p_Value3 * Leak
temp4=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     4
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value4(select=(s_PIndex4 == 1)) = p_Value4 * Leak
temp5=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     5
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value5(select=(s_PIndex5 == 1)) = p_Value5 * Leak
temp6=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     6
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value6(select=(s_PIndex6 == 1)) = p_Value6 * Leak
temp7=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     7
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value7(select=(s_PIndex7 == 1)) = p_Value7 * Leak
temp8=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     8
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value8(select=(s_PIndex8 == 1)) = p_Value8 * Leak
temp9=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     9
  # Setting values for parameter Leak
    # Substituting parameter values in zones
    p_Value9(select=(s_PIndex9 == 1)) = p_Value9 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_1.arrays';delim="space", &
  plist=p_Value1)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_2.arrays';delim="space", &
  plist=p_Value2)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_3.arrays';delim="space", &
  plist=p_Value3)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_4.arrays';delim="space", &
  plist=p_Value4)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_5.arrays';delim="space", &
  plist=p_Value5)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_6.arrays';delim="space", &
  plist=p_Value6)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_7.arrays';delim="space", &
  plist=p_Value7)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_8.arrays';delim="space", &
  plist=p_Value8)
write_column_data_file(header='no', &
  file='arrays\lakeTest.Lakebed_Leakance_9.arrays';delim="space", &
  plist=p_Value9)