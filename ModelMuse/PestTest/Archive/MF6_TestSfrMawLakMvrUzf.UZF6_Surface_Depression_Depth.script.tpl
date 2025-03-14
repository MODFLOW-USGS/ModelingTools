ptf @
#Script for PLPROC

#Read parameter values
UZF2 = @                        UZF2@
# Pilot points are not used with UZF2.

#Read MODFLOW 6 grid information file
cl_Discretization1 = read_mf6_grid_specs(file='MF6_TestSfrMawLakMvrUzf.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id1; layer=1, &
  plist_layer_bottom =bot1; layer=1, &
  plist_top = top)
cl_Discretization2 = read_mf6_grid_specs(file='MF6_TestSfrMawLakMvrUzf.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id2; layer=2, &
  plist_layer_bottom =bot2; layer=2, &
  )
cl_Discretization3 = read_mf6_grid_specs(file='MF6_TestSfrMawLakMvrUzf.dis.grb', &
  dimensions=2, &
  slist_layer_idomain=id3; layer=3, &
  plist_layer_bottom =bot3; layer=3, &
  )



# Layer     1

#Read data to modify
read_list_file(reference_clist='cl_Discretization1',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth.PstValues')

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization1,value=0.0)
# Setting values for layer     1
  # Setting values for parameter UZF2
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 11)) = p_Value1 * UZF2

#Write new data values
write_column_data_file(header='no', &
  file='arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_1.arrays';delim="space", &
  plist=p_Value1)

# Remove sLists and pLists
s_PIndex1.remove()
p_Value1.remove()
temp1.remove()

# Layer     2

#Read data to modify
read_list_file(reference_clist='cl_Discretization2',skiplines=1, &
  slist=s_PIndex2;column=4, &
  plist=p_Value2;column=5, &
  file='MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth.PstValues')

# Modfify data values
temp2=new_plist(reference_clist=cl_Discretization2,value=0.0)
# Setting values for layer     2
  # Setting values for parameter UZF2
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 11)) = p_Value2 * UZF2

#Write new data values
write_column_data_file(header='no', &
  file='arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_2.arrays';delim="space", &
  plist=p_Value2)

# Remove sLists and pLists
s_PIndex2.remove()
p_Value2.remove()
temp2.remove()

# Layer     3

#Read data to modify
read_list_file(reference_clist='cl_Discretization3',skiplines=1, &
  slist=s_PIndex3;column=6, &
  plist=p_Value3;column=7, &
  file='MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth.PstValues')

# Modfify data values
temp3=new_plist(reference_clist=cl_Discretization3,value=0.0)
# Setting values for layer     3
  # Setting values for parameter UZF2
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 11)) = p_Value3 * UZF2

#Write new data values
write_column_data_file(header='no', &
  file='arrays\MF6_TestSfrMawLakMvrUzf.UZF6_Surface_Depression_Depth_3.arrays';delim="space", &
  plist=p_Value3)

# Remove sLists and pLists
s_PIndex3.remove()
p_Value3.remove()
temp3.remove()

