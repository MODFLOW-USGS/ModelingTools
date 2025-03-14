ptf @
#Script for PLPROC

#Read parameter values
Sfr1 = @                        Sfr1@
# Pilot points are not used with Sfr1.

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
  file='MF6_TestSfrMawLakMvrUzf.SfrWidth.PstValues')

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization1,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Sfr1
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 5)) = p_Value1 * Sfr1

#Write new data values
write_column_data_file(header='no', &
  file='arrays\MF6_TestSfrMawLakMvrUzf.SfrWidth_1.arrays';delim="space", &
  plist=p_Value1)

# Remove sLists and pLists
s_PIndex1.remove()
p_Value1.remove()
temp1.remove()

