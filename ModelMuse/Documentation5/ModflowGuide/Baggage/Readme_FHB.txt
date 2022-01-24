As of MODFLOW-2000 version 1.11, the Flow and Head Boundary (FHB) Package 
includes support for the Layer Property Flow (LPF) and Hydrogeologic Unit 
Flow (HUF) Packages.  Addition of this support requires clarification of 
the explanation of the IFHBSS variable in Leake and Lilly (1997, p. 12).
The revised explanation follows.


IFHBSS is the FHB steady-state option flag.  If the simulation includes any 
transient-state stress periods, the flag is read but not used; in this case, 
specified-flow, specified-head, and auxiliary-variable values will be 
interpolated for steady-state stress periods in the same way that values are 
interpolated for transient stress periods.  If the simulation includes only 
steady-state stress periods, the flag controls how flow, head, and auxiliary-
variable values will be computed for each steady-state solution.

  If IFHBSS = 0, values of flow, head, and auxiliary variables will be those 
  specified for the starting time of the simulation.  This results in use of 
  he first value in arrays FLWRAT, SBHED, and AuxVar for each respective 
  boundary cell.

  If IFHBSS does not = 0, values of flow, head, and auxiliary variables will
  be interpolated in the same way that values are computed for transient 
  simulations.


REFERENCE:

Leake, S.A., and Lilly, M.R., 1997, Documentation of a computer program (FHB1)
for assignment of transient specified-flow and specified-head boundaries in
applications of the modular finite-difference ground-water flow model 
(MODFLOW): U.S. Geological Survey Open-File Report 97-571, 50 p.
