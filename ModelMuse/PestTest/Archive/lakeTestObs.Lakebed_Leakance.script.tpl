ptf @
#Script for PLPROC

#Read parameter values
Leak = @                        Leak@
# Pilot points are used with Leak.

#Read MODFLOW-2005 grid information file
cl_Discretization = read_mf_grid_specs(file="lakeTestObs.gsf")
# Layer     1

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 1
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_1.arrays';delim="space", &
  plist=p_Value1)

# Remove sLists and pLists
s_PIndex1.remove()
p_Value1.remove()
temp1.remove()

# Layer     2

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 2
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 1)) = p_Value2 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_2.arrays';delim="space", &
  plist=p_Value2)

# Remove sLists and pLists
s_PIndex2.remove()
p_Value2.remove()
temp2.remove()

# Layer     3

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 3
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 1)) = p_Value3 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_3.arrays';delim="space", &
  plist=p_Value3)

# Remove sLists and pLists
s_PIndex3.remove()
p_Value3.remove()
temp3.remove()

# Layer     4

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex4;column=8, &
  plist=p_Value4;column=9, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp4=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     4
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 4
    # Substituting parameter values in zones
    p_Value4(select=(s_PIndex4 == 1)) = p_Value4 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_4.arrays';delim="space", &
  plist=p_Value4)

# Remove sLists and pLists
s_PIndex4.remove()
p_Value4.remove()
temp4.remove()

# Layer     5

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex5;column=10, &
  plist=p_Value5;column=11, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp5=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     5
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 5
    # Substituting parameter values in zones
    p_Value5(select=(s_PIndex5 == 1)) = p_Value5 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_5.arrays';delim="space", &
  plist=p_Value5)

# Remove sLists and pLists
s_PIndex5.remove()
p_Value5.remove()
temp5.remove()

# Layer     6

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex6;column=12, &
  plist=p_Value6;column=13, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp6=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     6
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 6
    # Substituting parameter values in zones
    p_Value6(select=(s_PIndex6 == 1)) = p_Value6 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_6.arrays';delim="space", &
  plist=p_Value6)

# Remove sLists and pLists
s_PIndex6.remove()
p_Value6.remove()
temp6.remove()

# Layer     7

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex7;column=14, &
  plist=p_Value7;column=15, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp7=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     7
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 7
    # Substituting parameter values in zones
    p_Value7(select=(s_PIndex7 == 1)) = p_Value7 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_7.arrays';delim="space", &
  plist=p_Value7)

# Remove sLists and pLists
s_PIndex7.remove()
p_Value7.remove()
temp7.remove()

# Layer     8

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex8;column=16, &
  plist=p_Value8;column=17, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp8=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     8
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 8
    # Substituting parameter values in zones
    p_Value8(select=(s_PIndex8 == 1)) = p_Value8 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_8.arrays';delim="space", &
  plist=p_Value8)

# Remove sLists and pLists
s_PIndex8.remove()
p_Value8.remove()
temp8.remove()

# Layer     9

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex9;column=18, &
  plist=p_Value9;column=19, &
  file='lakeTestObs.Lakebed_Leakance.PstValues')

# Modfify data values
temp9=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     9
  # Setting values for parameter Leak
    # Substituting interpolated values
    # no interpolated values defined for parameter Leak in layer 9
    # Substituting parameter values in zones
    p_Value9(select=(s_PIndex9 == 1)) = p_Value9 * Leak

#Write new data values
write_column_data_file(header='no', &
  file='arrays\lakeTestObs.Lakebed_Leakance_9.arrays';delim="space", &
  plist=p_Value9)

# Remove sLists and pLists
s_PIndex9.remove()
p_Value9.remove()
temp9.remove()

