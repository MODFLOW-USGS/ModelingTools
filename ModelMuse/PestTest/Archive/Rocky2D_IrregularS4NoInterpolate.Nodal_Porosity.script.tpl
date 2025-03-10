ptf $
#Script for PLPROC

#Read parameter values
Density = $                        Density$
# Pilot points are not used with Density.

cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  id_type='indexed',file='Rocky2D_IrregularS4NoInterpolate.c_nod')

# Layer     1

#Read data to modify
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_PIndex1;column=2, &
  plist=p_Value1;column=3, &
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity.PstValues')

# Modfify data values
temp1=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     1
  # Setting values for parameter Density
    # Substituting parameter values in zones
    p_Value1(select=(s_PIndex1 == 1)) = p_Value1 * Density

#Write new data values
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity_1.arrays';delim="space", &
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
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity.PstValues')

# Modfify data values
temp2=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     2
  # Setting values for parameter Density
    # Substituting parameter values in zones
    p_Value2(select=(s_PIndex2 == 1)) = p_Value2 * Density

#Write new data values
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity_2.arrays';delim="space", &
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
  file='Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity.PstValues')

# Modfify data values
temp3=new_plist(reference_clist=cl_Discretization,value=0.0)
# Setting values for layer     3
  # Setting values for parameter Density
    # Substituting parameter values in zones
    p_Value3(select=(s_PIndex3 == 1)) = p_Value3 * Density

#Write new data values
write_column_data_file(header='no', &
  file='arrays\Rocky2D_IrregularS4NoInterpolate.Nodal_Porosity_3.arrays';delim="space", &
  plist=p_Value3)

# Remove sLists and pLists
s_PIndex3.remove()
p_Value3.remove()
temp3.remove()

