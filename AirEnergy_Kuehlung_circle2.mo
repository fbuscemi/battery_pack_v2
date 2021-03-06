model AirEnergy_Kuehlung_circle2
  replaceable package Medium = Modelica.Media.Water.StandardWaterOnePhase constrainedby Modelica.Media.Interfaces.PartialMedium;
  

  block PouchWaermemenge
    Modelica.Blocks.Interfaces.RealInput I annotation(
      Placement(visible = true, transformation(origin = {-120, 32}, extent = {{-20, -20}, {20, 20}}, rotation = 0), iconTransformation(origin = {-120, 32}, extent = {{-20, -20}, {20, 20}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealInput R annotation(
      Placement(visible = true, transformation(origin = {-122, -24}, extent = {{-20, -20}, {20, 20}}, rotation = 0), iconTransformation(origin = {-122, -24}, extent = {{-20, -20}, {20, 20}}, rotation = 0)));
    Modelica.Blocks.Interfaces.RealOutput Q annotation(
      Placement(visible = true, transformation(origin = {110, 2}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {110, 2}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  equation
    Q = (I ^ 2 * R)/4;
  end PouchWaermemenge;

  block Wand
    import SI = Modelica.SIunits;
    parameter Integer n(min = 1) = 1 "Segmentation perpendicular to heat conduction";
    //Geometry s
    parameter SI.Length s "Wall thickness";
    parameter SI.Area area_h "Heat transfer area";
    //Material properties
    parameter SI.Density rho_wall "Density of wall material";
    parameter SI.SpecificHeatCapacity c_wall "Specific heat capacity of wall material";
    parameter SI.ThermalConductivity k_wall "Thermal conductivity of wall material";
    parameter SI.Mass[n] m = fill(rho_wall * area_h * s / n, n) "Distribution of wall mass";
    //Initialization
    inner Modelica.Fluid.System system;
    parameter Modelica.Fluid.Types.Dynamics energyDynamics = system.energyDynamics "Formulation of energy balance" annotation(
      Evaluate = true,
      Dialog(tab = "Assumptions", group = "Dynamics"));
    parameter SI.Temperature T_start "Wall temperature start value";
    parameter SI.Temperature dT "Start value for port_b.T - port_a.T";
    //Temperatures
    SI.Temperature[n] Tb(each start = T_start + 0.5 * dT);
    SI.Temperature[n] Ta(each start = T_start - 0.5 * dT);
    SI.Temperature[n] T(start = ones(n) * T_start, each stateSelect = StateSelect.prefer) "Wall temperature";
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a[n] heatPort_a "Thermal port" annotation(
      Placement(transformation(extent = {{-20, 40}, {20, 60}})));
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a[n] heatPort_b "Thermal port" annotation(
      Placement(transformation(extent = {{-20, -40}, {20, -60}})));
  initial equation
    if energyDynamics == Modelica.Fluid.Types.Dynamics.SteadyStateInitial then
      der(T) = zeros(n);
    elseif energyDynamics == Modelica.Fluid.Types.Dynamics.FixedInitial then
      T = ones(n) * T_start;
    end if;
  equation
    for i in 1:n loop
      assert(m[i] > 0, "Wall has negative dimensions");
      if energyDynamics == Modelica.Fluid.Types.Dynamics.SteadyState then
        0 = heatPort_a[i].Q_flow + heatPort_b[i].Q_flow;
      else
        c_wall * m[i] * der(T[i]) = heatPort_a[i].Q_flow + heatPort_b[i].Q_flow;
      end if;
      heatPort_a[i].Q_flow = 2 * k_wall / s * (Ta[i] - T[i]) * area_h / n;
      heatPort_b[i].Q_flow = 2 * k_wall / s * (Tb[i] - T[i]) * area_h / n;
    end for;
    Ta = heatPort_a.T;
    Tb = heatPort_b.T;
    annotation(
      Icon(coordinateSystem(preserveAspectRatio = false, extent = {{-100, -100}, {100, 100}}), graphics = {Rectangle(extent = {{-100, 40}, {100, -40}}, fillColor = {95, 95, 95}, fillPattern = FillPattern.Forward), Text(extent = {{-82, 18}, {76, -18}}, textString = "%name")}),
      Documentation(revisions = "<html>
  <ul>
  <li><em>04 Mar 2006</em>
  by Katrin Pr&ouml;l&szlig;:<br>
     Model added to the Fluid library</li>
  </ul>
  </html>", info = "<html>
  Simple model of circular (or any other closed shape) wall to be used for pipe (or duct) models. Heat conduction is regarded one dimensional, capacitance is lumped at the arithmetic mean temperature. The spatial discretization (parameter <code>n</code>) is meant to correspond to a connected fluid model discretization.
  </html>"));
  end Wand;
  inner Modelica.Fluid.System system annotation(
    Placement(visible = true, transformation(origin = {-282, -36}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));

  Modelica.Fluid.Sources.MassFlowSource_T boundary(redeclare package Medium = Medium, T = 293.15, m_flow = 0.0037, nPorts = 1) annotation(
    Placement(visible = true, transformation(origin = {-266, -80}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));

  block PouchZelle
    Modelica.Blocks.Sources.Step Ladevorgang(height = -1, offset = 1, startTime = 600) annotation(
      Placement(visible = true, transformation(origin = {-68, 10}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Blocks.Math.Product product annotation(
      Placement(visible = true, transformation(origin = {-32, 20}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Blocks.Sources.Step I(height = -77.2, offset = 96.5, startTime = 90) annotation(
      Placement(visible = true, transformation(origin = {-100, 38}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Thermal.HeatTransfer.Components.HeatCapacitor heatCapacitor(C = 36.3, T(displayUnit = "degC", fixed = true, start = 293.15)) annotation(
      Placement(visible = true, transformation(origin = {108, 40}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Blocks.Sources.Step R(height = -0.001, offset = 0.0065, startTime = 90) annotation(
      Placement(visible = true, transformation(origin = {-100, -18}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Thermal.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow annotation(
      Placement(visible = true, transformation(origin = {48, 18}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    AirEnergy_Kuehlung_circle2.PouchWaermemenge pouchWaermemenge1 annotation(
      Placement(visible = true, transformation(origin = {16, 18}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Thermal.HeatTransfer.Components.Convection ZelleAufAlu annotation(
      Placement(visible = true, transformation(origin = {150, -12}, extent = {{-10, -10}, {10, 10}}, rotation = -90)));
    Modelica.Blocks.Sources.Constant Gc_Thermischer_Kontaktwiderstand(k = 3.311) annotation(
      Placement(visible = true, transformation(origin = {116, -24}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b Q annotation(
      Placement(visible = true, transformation(origin = {202, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {202, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_hinterThermischeLeitung annotation(
      Placement(visible = true, transformation(origin = {186, -6}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_vorHeatCapacitor annotation(
      Placement(visible = true, transformation(origin = {84, 4}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_nachHeatCapacitor annotation(
      Placement(visible = true, transformation(origin = {146, 30}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  inner Modelica.Fluid.System system annotation(
      Placement(visible = true, transformation(origin = {-92, 86}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  equation
    connect(I.y, product.u1) annotation(
      Line(points = {{-89, 38}, {-44, 38}, {-44, 26}}, color = {0, 0, 127}));
    connect(Ladevorgang.y, product.u2) annotation(
      Line(points = {{-57, 10}, {-44.5, 10}, {-44.5, 14}, {-44, 14}}, color = {0, 0, 127}));
    connect(pouchWaermemenge1.Q, prescribedHeatFlow.Q_flow) annotation(
      Line(points = {{27, 18}, {38, 18}}, color = {0, 0, 127}));
    connect(product.y, pouchWaermemenge1.I) annotation(
      Line(points = {{-20, 20}, {-8, 20}, {-8, 21}, {4, 21}}, color = {0, 0, 127}));
    connect(R.y, pouchWaermemenge1.R) annotation(
      Line(points = {{-88, -18}, {2, -18}, {2, 16}, {4, 16}}, color = {0, 0, 127}));
    connect(Gc_Thermischer_Kontaktwiderstand.y, ZelleAufAlu.Gc) annotation(
      Line(points = {{127, -24}, {134.5, -24}, {134.5, -12}, {160, -12}}, color = {0, 0, 127}));
    connect(heatFlowSensor_hinterThermischeLeitung.port_b, Q) annotation(
      Line(points = {{196, -6}, {204, -6}, {204, 22}, {202, 22}}, color = {191, 0, 0}));
    connect(heatFlowSensor_vorHeatCapacitor.port_b, heatCapacitor.port) annotation(
      Line(points = {{94, 4}, {94, 16}, {108, 16}, {108, 30}}, color = {191, 0, 0}));
    connect(prescribedHeatFlow.port, heatFlowSensor_vorHeatCapacitor.port_a) annotation(
      Line(points = {{58, 18}, {74, 18}, {74, 4}}, color = {191, 0, 0}));
    connect(ZelleAufAlu.fluid, heatFlowSensor_hinterThermischeLeitung.port_a) annotation(
      Line(points = {{150, -22}, {176, -22}, {176, -6}}, color = {191, 0, 0}));
    connect(heatCapacitor.port, heatFlowSensor_nachHeatCapacitor.port_a) annotation(
      Line(points = {{108, 30}, {136, 30}}, color = {191, 0, 0}));
    connect(heatFlowSensor_nachHeatCapacitor.port_b, ZelleAufAlu.solid) annotation(
      Line(points = {{156, 30}, {150, 30}, {150, -2}}, color = {191, 0, 0}));
    annotation(
      Diagram(graphics = {Rectangle(origin = {-49, 17}, extent = {{-71, 47}, {267, -57}}), Text(origin = {-86, 63}, extent = {{-30, 5}, {-2, -11}}, textString = "Pouch Zelle")}, coordinateSystem(initialScale = 0.1)));
  end PouchZelle;

  model CooledPouchCell
    AirEnergy_Kuehlung_circle2.PouchZelle pouchZelle1 annotation(
      Placement(visible = true, transformation(origin = {-71, 13}, extent = {{-11, -11}, {11, 11}}, rotation = -90)));
    AirEnergy_Kuehlung_circle2.Wand wand( T(displayUnit = "degC", fixed = false),T_start (displayUnit = "K") = 293.15, Ta(fixed = false), Tb(fixed = false),area_h = 3.44e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, n = 1, rho_wall = 2700, s = 0.002) annotation(
      Placement(visible = true, transformation(origin = {-70, -26}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a Qpipe annotation(
      Placement(visible = true, transformation(origin = {-18, -80}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-18, -80}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  inner Modelica.Fluid.System system annotation(
      Placement(visible = true, transformation(origin = {-86, 82}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchZelle pouchZelle annotation(
      Placement(visible = true, transformation(origin = {36, 20}, extent = {{-10, -10}, {10, 10}}, rotation = -90)));
  AirEnergy_Kuehlung_circle2.Wand wand1(T_start(displayUnit = "K") = 293.15, area_h = 3.44e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.002)  annotation(
      Placement(visible = true, transformation(origin = {38, -24}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Components.ThermalCollector thermalCollector annotation(
      Placement(visible = true, transformation(origin = {-16, -48}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  equation
  connect(pouchZelle1.Q, wand.heatPort_a[1]) annotation(
      Line(points = {{-69, -9}, {-69, -14.5}, {-70, -14.5}, {-70, -21}}, color = {191, 0, 0}));
  connect(pouchZelle.Q, wand1.heatPort_a[1]) annotation(
      Line(points = {{38, 0}, {38, -19}}, color = {191, 0, 0}));
  connect(thermalCollector.port_b, Qpipe) annotation(
      Line(points = {{-16, -58}, {-16, -58}, {-16, -80}, {-18, -80}}, color = {191, 0, 0}));
  connect(wand1.heatPort_b[1], thermalCollector.port_a[1]) annotation(
      Line(points = {{38, -30}, {-16, -30}, {-16, -38}, {-16, -38}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand.heatPort_b[1], thermalCollector.port_a[2]) annotation(
      Line(points = {{-70, -31}, {-16, -31}, {-16, -38}}, color = {191, 0, 0}, thickness = 0.5));
  end CooledPouchCell;

  block TwoPouchCells
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell annotation(
      Placement(visible = true, transformation(origin = {-82, 30}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell1 annotation(
      Placement(visible = true, transformation(origin = {28, 30}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Components.ThermalCollector thermalCollector(m = 2)  annotation(
      Placement(visible = true, transformation(origin = {-28, -38}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b Qtwocells annotation(
      Placement(visible = true, transformation(origin = {-28, -100}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-28, -100}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor1_vorTC annotation(
      Placement(visible = true, transformation(origin = {0, 8}, extent = {{-10, -10}, {10, 10}}, rotation = 90)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor2_vorTC annotation(
      Placement(visible = true, transformation(origin = {-50, 10}, extent = {{-10, -10}, {10, 10}}, rotation = 90)));
  Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_hinterTC annotation(
      Placement(visible = true, transformation(origin = {-30, -70}, extent = {{-10, -10}, {10, 10}}, rotation = 90)));
  inner Modelica.Fluid.System system annotation(
      Placement(visible = true, transformation(origin = {-86, 80}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  equation
  connect(cooledPouchCell1.Qpipe, heatFlowSensor1_vorTC.port_b) annotation(
      Line(points = {{26, 22}, {13, 22}, {13, 18}, {0, 18}}, color = {191, 0, 0}));
  connect(cooledPouchCell.Qpipe, heatFlowSensor2_vorTC.port_b) annotation(
      Line(points = {{-84, 22}, {-50, 22}, {-50, 20}, {-50, 20}}, color = {191, 0, 0}));
  connect(heatFlowSensor2_vorTC.port_a, thermalCollector.port_a[1]) annotation(
      Line(points = {{-50, 0}, {-28, 0}, {-28, -28}, {-28, -28}}, color = {191, 0, 0}));
  connect(heatFlowSensor1_vorTC.port_a, thermalCollector.port_a[2]) annotation(
      Line(points = {{0, -2}, {-28, -2}, {-28, -28}, {-28, -28}}, color = {191, 0, 0}));
  connect(thermalCollector.port_b, heatFlowSensor_hinterTC.port_b) annotation(
      Line(points = {{-28, -48}, {-30, -48}, {-30, -60}, {-30, -60}}, color = {191, 0, 0}));
  connect(heatFlowSensor_hinterTC.port_a, Qtwocells) annotation(
      Line(points = {{-30, -80}, {-26, -80}, {-26, -100}, {-28, -100}}, color = {191, 0, 0}));
  end TwoPouchCells;

  block PouchCellAndPipe
    Modelica.Fluid.Pipes.DynamicPipe pipe1(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true) annotation(
      Placement(visible = true, transformation(origin = {-28, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_a port_a (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {-118, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-118, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_b port_b (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {152, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {152, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_a fluidPort_a (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {-126, 62}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-126, 62}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_b fluidPort_b (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {152, 62}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {152, 62}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Pipes.DynamicPipe dynamicPipe(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true) annotation(
      Placement(visible = true, transformation(origin = {-28, 64}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  inner Modelica.Fluid.System system annotation(
      Placement(visible = true, transformation(origin = {-124, 92}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.Wand wand1(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall (displayUnit = "kg/m3") = 2700, s = 0.04)  annotation(
      Placement(visible = true, transformation(origin = {-50, 42}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell annotation(
      Placement(visible = true, transformation(origin = {18, 86}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell1 annotation(
      Placement(visible = true, transformation(origin = {28, -52}, extent = {{-10, -10}, {10, 10}}, rotation = 180)));
  Modelica.Fluid.Pipes.DynamicPipe dynamicPipe1(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true) annotation(
      Placement(visible = true, transformation(origin = {-28, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.Wand wand(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.04) annotation(
      Placement(visible = true, transformation(origin = {-50, 0}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Pipes.DynamicPipe dynamicPipe2(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true) annotation(
      Placement(visible = true, transformation(origin = {-28, -16}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.Wand wand2(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.04) annotation(
      Placement(visible = true, transformation(origin = {-48, -34}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_a port_a1 (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {-118, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-118, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_a port_a2 (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {-120, -16}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {-120, -16}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_b port_b1 (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {138, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {138, 22}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Interfaces.FluidPort_b port_b2 (replaceable package Medium = Medium) annotation(
      Placement(visible = true, transformation(origin = {146, -18}, extent = {{-10, -10}, {10, 10}}, rotation = 0), iconTransformation(origin = {146, -18}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell2 annotation(
      Placement(visible = true, transformation(origin = {28, 42}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell3 annotation(
      Placement(visible = true, transformation(origin = {28, -6}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  equation
    connect(pipe1.port_b, port_b) annotation(
      Line(points = {{-18, -52}, {152, -52}}, color = {0, 127, 255}));
    connect(pipe1.port_a, port_a) annotation(
      Line(points = {{-38, -52}, {-118, -52}}, color = {0, 127, 255}));
  connect(dynamicPipe.port_a, fluidPort_a) annotation(
      Line(points = {{-38, 64}, {-91, 64}, {-91, 62}, {-126, 62}}, color = {0, 127, 255}));
  connect(dynamicPipe.port_b, fluidPort_b) annotation(
      Line(points = {{-18, 64}, {58, 64}, {58, 62}, {152, 62}}, color = {0, 127, 255}));
  connect(cooledPouchCell.Qpipe, dynamicPipe.heatPorts[1]) annotation(
      Line(points = {{16, 78}, {16, 77}, {-28, 77}, {-28, 68}}, color = {191, 0, 0}));
  connect(dynamicPipe1.port_b, port_b1) annotation(
      Line(points = {{-18, 22}, {138, 22}}, color = {0, 127, 255}));
  connect(dynamicPipe2.port_b, port_b2) annotation(
      Line(points = {{-18, -16}, {29, -16}, {29, -18}, {146, -18}}, color = {0, 127, 255}));
  connect(dynamicPipe1.port_a, port_a1) annotation(
      Line(points = {{-38, 22}, {-116, 22}, {-116, 22}, {-118, 22}}, color = {0, 127, 255}));
  connect(dynamicPipe2.port_a, port_a2) annotation(
      Line(points = {{-38, -16}, {-116, -16}, {-116, -16}, {-120, -16}}, color = {0, 127, 255}));
  connect(wand1.heatPort_a[1], dynamicPipe.heatPorts[2]) annotation(
      Line(points = {{-50, 47}, {-50, 57}, {-28, 57}, {-28, 68}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand1.heatPort_b[1], dynamicPipe1.heatPorts[1]) annotation(
      Line(points = {{-50, 37}, {-50, 31}, {-28, 31}, {-28, 26}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand.heatPort_a[1], dynamicPipe1.heatPorts[2]) annotation(
      Line(points = {{-50, 5}, {-50, 16}, {-28, 16}, {-28, 26}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand.heatPort_b[1], dynamicPipe2.heatPorts[1]) annotation(
      Line(points = {{-50, -5}, {-50, -8}, {-28, -8}, {-28, -12}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand2.heatPort_a[1], dynamicPipe2.heatPorts[2]) annotation(
      Line(points = {{-48, -29}, {-48, -20}, {-28, -20}, {-28, -12}}, color = {191, 0, 0}, thickness = 0.5));
  connect(wand2.heatPort_b[1], pipe1.heatPorts[1]) annotation(
      Line(points = {{-48, -39}, {-48, -43}, {-28, -43}, {-28, -48}}, color = {191, 0, 0}, thickness = 0.5));
  connect(cooledPouchCell1.Qpipe, pipe1.heatPorts[2]) annotation(
      Line(points = {{30, -44}, {0, -44}, {0, -48}, {-28, -48}}, color = {191, 0, 0}));
  connect(cooledPouchCell2.Qpipe, dynamicPipe1.heatPorts[3]) annotation(
      Line(points = {{26, 34}, {-26, 34}, {-26, 26}, {-28, 26}}, color = {191, 0, 0}));
  connect(cooledPouchCell3.Qpipe, dynamicPipe2.heatPorts[3]) annotation(
      Line(points = {{26, -14}, {2, -14}, {2, -12}, {-28, -12}}, color = {191, 0, 0}));
  end PouchCellAndPipe;

  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe annotation(
    Placement(visible = true, transformation(origin = {-168, -98}, extent = {{-16, -16}, {16, 16}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe1 annotation(
    Placement(visible = true, transformation(origin = {-98, -98}, extent = {{-16, -16}, {16, 16}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe2 annotation(
    Placement(visible = true, transformation(origin = {-37, -97}, extent = {{-17, -17}, {17, 17}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe3 annotation(
    Placement(visible = true, transformation(origin = {42, -96}, extent = {{-18, -18}, {18, 18}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe4 annotation(
    Placement(visible = true, transformation(origin = {122, -96}, extent = {{-18, -18}, {18, 18}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe5 annotation(
    Placement(visible = true, transformation(origin = {207, -95}, extent = {{-17, -17}, {17, 17}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe6 annotation(
    Placement(visible = true, transformation(origin = {278, -92}, extent = {{-18, -18}, {18, 18}}, rotation = 0)));
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe7 annotation(
    Placement(visible = true, transformation(origin = {353, -93}, extent = {{-19, -19}, {19, 19}}, rotation = 0)));
  Modelica.Fluid.Sensors.TemperatureTwoPort KuehlmediumTempOut(replaceable package Medium = Medium) annotation(
    Placement(visible = true, transformation(origin = {-206, -138}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Sensors.TemperatureTwoPort KuehlmediumTempIn (replaceable package Medium = Medium) annotation(
    Placement(visible = true, transformation(origin = {-212, -68}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));
  Modelica.Fluid.Sources.FixedBoundary boundary1(replaceable package Medium = Medium, T = 293.15, nPorts = 1) annotation(
    Placement(visible = true, transformation(origin = {-268, -138}, extent = {{-10, -10}, {10, 10}}, rotation = 0)));


equation
  connect(pouchCellAndPipe.fluidPort_b, pouchCellAndPipe1.fluidPort_a) annotation(
    Line(points = {{-144, -88}, {-118, -88}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe1.fluidPort_b, pouchCellAndPipe2.fluidPort_a) annotation(
    Line(points = {{-74, -88}, {-66, -88}, {-66, -86}, {-58, -86}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe1.port_a, pouchCellAndPipe.port_b) annotation(
    Line(points = {{-117, -106}, {-144, -106}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe2.port_a, pouchCellAndPipe1.port_b) annotation(
    Line(points = {{-57, -106}, {-74, -106}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe.port_a, KuehlmediumTempOut.port_b) annotation(
    Line(points = {{-187, -106}, {-187, -118.5}, {-196, -118.5}, {-196, -138}}, color = {0, 127, 255}));
  connect(boundary.ports[1], KuehlmediumTempIn.port_a) annotation(
    Line(points = {{-256, -80}, {-236, -80}, {-236, -68}, {-222, -68}}, color = {0, 127, 255}));
  connect(KuehlmediumTempIn.port_b, pouchCellAndPipe.fluidPort_a) annotation(
    Line(points = {{-202, -68}, {-188, -68}, {-188, -88}}, color = {0, 127, 255}));
  connect(KuehlmediumTempOut.port_a, boundary1.ports[1]) annotation(
    Line(points = {{-216, -138}, {-258, -138}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe2.fluidPort_b, pouchCellAndPipe3.fluidPort_a) annotation(
    Line(points = {{-12, -86}, {20, -86}, {20, -84}, {20, -84}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe3.fluidPort_b, pouchCellAndPipe4.fluidPort_a) annotation(
    Line(points = {{70, -84}, {98, -84}, {98, -84}, {100, -84}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe4.fluidPort_b, pouchCellAndPipe5.fluidPort_a) annotation(
    Line(points = {{150, -84}, {186, -84}, {186, -84}, {186, -84}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe5.fluidPort_b, pouchCellAndPipe6.fluidPort_a) annotation(
    Line(points = {{232, -84}, {256, -84}, {256, -80}, {256, -80}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe6.fluidPort_b, pouchCellAndPipe7.fluidPort_a) annotation(
    Line(points = {{306, -80}, {328, -80}, {328, -82}, {330, -82}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe7.fluidPort_b, pouchCellAndPipe7.port_b1) annotation(
    Line(points = {{382, -82}, {378, -82}, {378, -88}, {380, -88}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe7.port_a1, pouchCellAndPipe6.port_b1) annotation(
    Line(points = {{330, -88}, {302, -88}, {302, -88}, {302, -88}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe6.port_a1, pouchCellAndPipe5.port_b1) annotation(
    Line(points = {{256, -88}, {232, -88}, {232, -92}, {230, -92}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe5.port_a1, pouchCellAndPipe4.port_b1) annotation(
    Line(points = {{186, -92}, {146, -92}, {146, -92}, {146, -92}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe4.port_a1, pouchCellAndPipe3.port_b1) annotation(
    Line(points = {{100, -92}, {68, -92}, {68, -92}, {66, -92}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe3.port_a1, pouchCellAndPipe2.port_b1) annotation(
    Line(points = {{20, -92}, {-14, -92}, {-14, -94}, {-14, -94}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe2.port_a1, pouchCellAndPipe1.port_b1) annotation(
    Line(points = {{-58, -94}, {-76, -94}, {-76, -94}, {-76, -94}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe1.port_a1, pouchCellAndPipe.port_b1) annotation(
    Line(points = {{-116, -94}, {-146, -94}, {-146, -94}, {-146, -94}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe.port_a1, pouchCellAndPipe.port_a2) annotation(
    Line(points = {{-186, -94}, {-188, -94}, {-188, -100}, {-188, -100}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe.port_b2, pouchCellAndPipe1.port_a2) annotation(
    Line(points = {{-144, -100}, {-116, -100}, {-116, -100}, {-118, -100}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe1.port_b2, pouchCellAndPipe2.port_a2) annotation(
    Line(points = {{-74, -100}, {-58, -100}, {-58, -100}, {-58, -100}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe2.port_b2, pouchCellAndPipe3.port_a2) annotation(
    Line(points = {{-12, -100}, {20, -100}, {20, -98}, {20, -98}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe3.port_b2, pouchCellAndPipe4.port_a2) annotation(
    Line(points = {{68, -100}, {100, -100}, {100, -98}, {100, -98}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe4.port_b2, pouchCellAndPipe5.port_a2) annotation(
    Line(points = {{148, -100}, {188, -100}, {188, -98}, {186, -98}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe5.port_b2, pouchCellAndPipe6.port_a2) annotation(
    Line(points = {{232, -98}, {256, -98}, {256, -94}, {256, -94}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe6.port_b2, pouchCellAndPipe7.port_a2) annotation(
    Line(points = {{304, -96}, {328, -96}, {328, -96}, {330, -96}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe7.port_b2, pouchCellAndPipe7.port_b) annotation(
    Line(points = {{380, -96}, {382, -96}, {382, -102}, {382, -102}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe7.port_a, pouchCellAndPipe6.port_b) annotation(
    Line(points = {{330, -102}, {306, -102}, {306, -102}, {306, -102}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe6.port_a, pouchCellAndPipe5.port_b) annotation(
    Line(points = {{256, -102}, {234, -102}, {234, -104}, {232, -104}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe5.port_a, pouchCellAndPipe4.port_b) annotation(
    Line(points = {{186, -104}, {150, -104}, {150, -106}, {150, -106}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe4.port_a, pouchCellAndPipe3.port_b) annotation(
    Line(points = {{100, -106}, {70, -106}, {70, -106}, {70, -106}}, color = {0, 127, 255}));
  connect(pouchCellAndPipe3.port_a, pouchCellAndPipe2.port_b) annotation(
    Line(points = {{20, -106}, {-10, -106}, {-10, -106}, {-12, -106}}, color = {0, 127, 255}));
  annotation(
    uses(Modelica(version = "3.2.3"), ElectricalEnergyStorage(version = "3.2.2")),
    Diagram(coordinateSystem(preserveAspectRatio = false, initialScale = 0.2, extent = {{-300, -300}, {300, 300}}), graphics = {Text(extent = {{-84, -16}, {-84, -16}}, textString = "Kühlkreislauf"), Text(extent = {{-88, -10}, {-88, -10}}, textString = "Kühlkreislauf", fontSize = 18), Text(origin = {-212, -19}, extent = {{-30, 5}, {142, -25}}, textString = "Kühlkreislauf Strangpressprofil ref. Airenergy")}),
    Icon(coordinateSystem(preserveAspectRatio = false, initialScale = 0.2, extent = {{-300, -300}, {300, 300}})),
    version = "",
    experiment(StartTime = 0, StopTime = 1000, Tolerance = 1e-06, Interval = 1));
end AirEnergy_Kuehlung_circle2;