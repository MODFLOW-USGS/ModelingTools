ptf @
#Script for PLPROC

#Read MODFLOW-2005 grid information file
cl_Discretization = read_mf_grid_specs(file="Drntest2.gsf")
#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='Drntest2.test.PstValues')

#Read parameter values
DrCon2 = @                        DrCon2@
# Pilot points are not used with DrCon2.

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter DrCon2
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * DrCon2

#Write new data values
write_column_data_file(header='no', &
  file='arrays\Drntest2.test_1.arrays';delim="space", &
  plist=p_Value1)