ptf $
#Script for PLPROC

#Read parameter values
Density = $                        Density$
#Density = 1.1

#Read SUTRA node information file
cl_Discretization = read_list_file(skiplines=1,dimensions=2, &
  plist=p_x;column=2, &
  plist=p_y;column=3, &
  slist=s_NN2D;column=4, &
  id_type='indexed',file='Rocky2D_IrregularS4Interpolate.c_nod')

temp=new_plist(reference_clist=cl_Discretization,value=0.0)
#Read pilot point data
POR_PilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='POR_Density_1';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Nodal_Porosity.Density.1.pp')
POR_PilotPoints2 = read_list_file(skiplines=0,dimensions=2, &
  plist='POR_Density_2';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Nodal_Porosity.Density.2.pp')
POR_PilotPoints3 = read_list_file(skiplines=0,dimensions=2, &
  plist='POR_Density_3';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Nodal_Porosity.Density.3.pp')
COMPMA_PilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='COMPMA_Density_1';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Matrix_Compressibility.Density.1.pp')
COMPMA_PilotPoints2 = read_list_file(skiplines=0,dimensions=2, &
  plist='COMPMA_Density_2';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Matrix_Compressibility.Density.2.pp')
COMPMA_PilotPoints3 = read_list_file(skiplines=0,dimensions=2, &
  plist='COMPMA_Density_3';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Matrix_Compressibility.Density.3.pp')
CS_PilotPoints1 = read_list_file(skiplines=0,dimensions=2, &
  plist='CS_Density_1';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Grain_Specific_Heat.Density.1.pp')
CS_PilotPoints2 = read_list_file(skiplines=0,dimensions=2, &
  plist='CS_Density_2';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Grain_Specific_Heat.Density.2.pp')
CS_PilotPoints3 = read_list_file(skiplines=0,dimensions=2, &
  plist='CS_Density_3';column=5, &
  id_type='character',file='Rocky2D_IrregularS4Interpolate.14B.Solid_Grain_Specific_Heat.Density.3.pp')

# Read data to modify
# Layer     1

read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z1;column=5, &
  slist=s_NN3D1;column=6, &
  slist=s_Active_1;column=7, &
  id_type='indexed',file='Rocky2D_IrregularS4Interpolate.c_nod')


# Read porosity
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer1;column=3, &
  plist=p_Porosity1;column=4, &
  slist=s_PorPar1;column=5, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity')

# Read Unsaturated Zone
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region1;column=4, &
  file='Rocky2D_IrregularS4Interpolate.Region_Nodes')

# Read COMPMA
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA1;column=4, &
  slist=s_COMPMAPar1;column=5, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')

# Read CS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS1;column=4, &
  slist=s_CSPar1;column=5, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')

# Read RHOS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS1;column=4, &
  slist=s_RHOSPar1;column=5, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')

# applying parameter values
# applying parameter Density
    # Substituting interpolated values
    # Get interpolated values

    # Substituting interpolated values
    # Get interpolated values

    # Substituting interpolated values
    # Get interpolated values

    # Substituting parameter values in zones
p_RHOS1(select=(s_RHOSPar1 == 1)) = p_RHOS1 * Density

# removing unneeded slists.
s_PorPar1.remove()
s_COMPMAPar1.remove()
s_CSPar1.remove()
s_RHOSPar1.remove()

# Write new data values
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4Interpolate.14B_1';delim="space", &
  select=(s_Active_1 == 1), &
  slist='s_NN3D1', &
  slist=s_Unsat_Region1, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z1, &
  plist=p_Porosity1, &
  plist=p_COMPMA1, &
  plist=p_CS1, &
  plist=p_RHOS1)

# remove unneeded slists and plists.
s_Active_1.remove()
s_NN3D1.remove()
s_Unsat_Region1.remove()
p_z1.remove()
p_Porosity1.remove()
p_COMPMA1.remove()
p_CS1.remove()
p_RHOS1.remove()

# Layer     2

read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z2;column=8, &
  slist=s_NN3D2;column=9, &
  slist=s_Active_2;column=10, &
  id_type='indexed',file='Rocky2D_IrregularS4Interpolate.c_nod')


# Read porosity
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer2;column=7, &
  plist=p_Porosity2;column=8, &
  slist=s_PorPar2;column=9, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity')

# Read Unsaturated Zone
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region2;column=7, &
  file='Rocky2D_IrregularS4Interpolate.Region_Nodes')

# Read COMPMA
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA2;column=8, &
  slist=s_COMPMAPar2;column=9, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')

# Read CS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS2;column=8, &
  slist=s_CSPar2;column=9, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')

# Read RHOS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS2;column=8, &
  slist=s_RHOSPar2;column=9, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')

# applying parameter values
# applying parameter Density
    # Substituting interpolated values
    # Get interpolated values

    # Substituting interpolated values
    # Get interpolated values

    # Substituting interpolated values
    # Get interpolated values

    # Substituting parameter values in zones
p_RHOS2(select=(s_RHOSPar2 == 1)) = p_RHOS2 * Density

# removing unneeded slists.
s_PorPar2.remove()
s_COMPMAPar2.remove()
s_CSPar2.remove()
s_RHOSPar2.remove()

# Write new data values
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4Interpolate.14B_2';delim="space", &
  select=(s_Active_2 == 1), &
  slist='s_NN3D2', &
  slist=s_Unsat_Region2, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z2, &
  plist=p_Porosity2, &
  plist=p_COMPMA2, &
  plist=p_CS2, &
  plist=p_RHOS2)

# remove unneeded slists and plists.
s_Active_2.remove()
s_NN3D2.remove()
s_Unsat_Region2.remove()
p_z2.remove()
p_Porosity2.remove()
p_COMPMA2.remove()
p_CS2.remove()
p_RHOS2.remove()

# Layer     3

read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_z3;column=11, &
  slist=s_NN3D3;column=12, &
  slist=s_Active_3;column=13, &
  id_type='indexed',file='Rocky2D_IrregularS4Interpolate.c_nod')


# Read porosity
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Layer3;column=11, &
  plist=p_Porosity3;column=12, &
  slist=s_PorPar3;column=13, &
  file='Rocky2D_IrregularS4Interpolate.Nodal_Porosity')

# Read Unsaturated Zone
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  slist=s_Unsat_Region3;column=10, &
  file='Rocky2D_IrregularS4Interpolate.Region_Nodes')

# Read COMPMA
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_COMPMA3;column=12, &
  slist=s_COMPMAPar3;column=13, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Matrix_Compressibility')

# Read CS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_CS3;column=12, &
  slist=s_CSPar3;column=13, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Specific_Heat')

# Read RHOS
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')
read_list_file(reference_clist='cl_Discretization',skiplines=1, &
  plist=p_RHOS3;column=12, &
  slist=s_RHOSPar3;column=13, &
  file='Rocky2D_IrregularS4Interpolate.Solid_Grain_Density')

# applying parameter values
# applying parameter Density
    # Substituting interpolated values
    # no interpolated values defined for parameter Density in layer 3

    # Substituting interpolated values
    # no interpolated values defined for parameter Density in layer 3

    # Substituting interpolated values
    # no interpolated values defined for parameter Density in layer 3

    # Substituting parameter values in zones
p_RHOS3(select=(s_RHOSPar3 == 1)) = p_RHOS3 * Density

# removing unneeded slists.
s_PorPar3.remove()
s_COMPMAPar3.remove()
s_CSPar3.remove()
s_RHOSPar3.remove()

# Write new data values
write_column_data_file(header = 'no', &
  file='Rocky2D_IrregularS4Interpolate.14B_3';delim="space", &
  select=(s_Active_3 == 1), &
  slist='s_NN3D3', &
  slist=s_Unsat_Region3, &
  plist=p_x, &
  plist=p_y, &
  plist=p_z3, &
  plist=p_Porosity3, &
  plist=p_COMPMA3, &
  plist=p_CS3, &
  plist=p_RHOS3)

# remove unneeded slists and plists.
s_Active_3.remove()
s_NN3D3.remove()
s_Unsat_Region3.remove()
p_z3.remove()
p_Porosity3.remove()
p_COMPMA3.remove()
p_CS3.remove()
p_RHOS3.remove()

