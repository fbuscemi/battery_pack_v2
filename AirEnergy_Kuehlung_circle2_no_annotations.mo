model AirEnergy_Kuehlung_circle2
  replaceable package Medium = Modelica.Media.Water.StandardWaterOnePhase constrainedby Modelica.Media.Interfaces.PartialMedium;
  
  block PouchWaermemenge
    Modelica.Blocks.Interfaces.RealInput I;
    Modelica.Blocks.Interfaces.RealInput R;
    Modelica.Blocks.Interfaces.RealOutput Q;
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
    parameter Modelica.Fluid.Types.Dynamics energyDynamics = system.energyDynamics "Formulation of energy balance";
    parameter SI.Temperature T_start "Wall temperature start value";
    parameter SI.Temperature dT "Start value for port_b.T - port_a.T";
    //Temperatures
    SI.Temperature[n] Tb(each start = T_start + 0.5 * dT);
    SI.Temperature[n] Ta(each start = T_start - 0.5 * dT);
    SI.Temperature[n] T(start = ones(n) * T_start, each stateSelect = StateSelect.prefer) "Wall temperature";
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a[n] heatPort_a;
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a[n] heatPort_b;
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
  end Wand;
  
  inner Modelica.Fluid.System system;
  Modelica.Fluid.Sources.MassFlowSource_T boundary(redeclare package Medium = Medium, T = 293.15, m_flow = 0.0037, nPorts = 1);

  block PouchZelle
    Modelica.Blocks.Sources.Step Ladevorgang(height = -1, offset = 1, startTime = 600);
    Modelica.Blocks.Math.Product product;
    Modelica.Blocks.Sources.Step I(height = -77.2, offset = 96.5, startTime = 90);
    Modelica.Thermal.HeatTransfer.Components.HeatCapacitor heatCapacitor(C = 36.3, T(displayUnit = "degC", fixed = true, start = 293.15));
    Modelica.Blocks.Sources.Step R(height = -0.001, offset = 0.0065, startTime = 90);
    Modelica.Thermal.HeatTransfer.Sources.PrescribedHeatFlow prescribedHeatFlow;
    AirEnergy_Kuehlung_circle2.PouchWaermemenge pouchWaermemenge1;
    Modelica.Thermal.HeatTransfer.Components.Convection ZelleAufAlu;
    Modelica.Blocks.Sources.Constant Gc_Thermischer_Kontaktwiderstand(k = 3.311);
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b Q;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_hinterThermischeLeitung;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_vorHeatCapacitor;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_nachHeatCapacitor;
    inner Modelica.Fluid.System system;
  equation
    connect(I.y, product.u1);
    connect(Ladevorgang.y, product.u2);
    connect(pouchWaermemenge1.Q, prescribedHeatFlow.Q_flow);
    connect(product.y, pouchWaermemenge1.I);
    connect(R.y, pouchWaermemenge1.R);
    connect(Gc_Thermischer_Kontaktwiderstand.y, ZelleAufAlu.Gc);
    connect(heatFlowSensor_hinterThermischeLeitung.port_b, Q);
    connect(heatFlowSensor_vorHeatCapacitor.port_b, heatCapacitor.port);
    connect(prescribedHeatFlow.port, heatFlowSensor_vorHeatCapacitor.port_a);
    connect(ZelleAufAlu.fluid, heatFlowSensor_hinterThermischeLeitung.port_a);
    connect(heatCapacitor.port, heatFlowSensor_nachHeatCapacitor.port_a);
    connect(heatFlowSensor_nachHeatCapacitor.port_b, ZelleAufAlu.solid);
  end PouchZelle;

  model CooledPouchCell
    AirEnergy_Kuehlung_circle2.PouchZelle pouchZelle1;
    AirEnergy_Kuehlung_circle2.Wand wand( T(displayUnit = "degC", fixed = false),T_start (displayUnit = "K") = 293.15, Ta(fixed = false), Tb(fixed = false),area_h = 3.44e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, n = 1, rho_wall = 2700, s = 0.002);
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_a Qpipe;
    inner Modelica.Fluid.System system;
    AirEnergy_Kuehlung_circle2.PouchZelle pouchZelle;
    AirEnergy_Kuehlung_circle2.Wand wand1(T_start(displayUnit = "K") = 293.15, area_h = 3.44e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.002);
    Modelica.Thermal.HeatTransfer.Components.ThermalCollector thermalCollector;
  equation
    connect(pouchZelle1.Q, wand.heatPort_a[1]);
    connect(pouchZelle.Q, wand1.heatPort_a[1]);
    connect(thermalCollector.port_b, Qpipe);
    connect(wand1.heatPort_b[1], thermalCollector.port_a[1]);
    connect(wand.heatPort_b[1], thermalCollector.port_a[2]);
  end CooledPouchCell;

  block TwoPouchCells
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell;
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell1;
    Modelica.Thermal.HeatTransfer.Components.ThermalCollector thermalCollector(m = 2);
    Modelica.Thermal.HeatTransfer.Interfaces.HeatPort_b Qtwocells;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor1_vorTC;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor2_vorTC;
    Modelica.Thermal.HeatTransfer.Sensors.HeatFlowSensor heatFlowSensor_hinterTC;
    inner Modelica.Fluid.System system;
  equation
    connect(cooledPouchCell1.Qpipe, heatFlowSensor1_vorTC.port_b);
    connect(cooledPouchCell.Qpipe, heatFlowSensor2_vorTC.port_b);
    connect(heatFlowSensor2_vorTC.port_a, thermalCollector.port_a[1]);
    connect(heatFlowSensor1_vorTC.port_a, thermalCollector.port_a[2]);
    connect(thermalCollector.port_b, heatFlowSensor_hinterTC.port_b);
    connect(heatFlowSensor_hinterTC.port_a, Qtwocells);
  end TwoPouchCells;

  block PouchCellAndPipe
    Modelica.Fluid.Pipes.DynamicPipe pipe1(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true);
    Modelica.Fluid.Interfaces.FluidPort_a port_a (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_b port_b (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_a fluidPort_a (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_b fluidPort_b (replaceable package Medium = Medium);
    Modelica.Fluid.Pipes.DynamicPipe dynamicPipe(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true);
    inner Modelica.Fluid.System system;
    AirEnergy_Kuehlung_circle2.Wand wand1(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.04);
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell;
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell1;
    Modelica.Fluid.Pipes.DynamicPipe dynamicPipe1(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true);
    AirEnergy_Kuehlung_circle2.Wand wand(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.04);
    Modelica.Fluid.Pipes.DynamicPipe dynamicPipe2(replaceable package Medium = Medium, T_start = 293.15, crossArea = 0.32e-4, diameter = 0.0, isCircular = false, length = 0.043, nNodes = 3, nParallel = 1, perimeter = 0.024, roughness = 0, use_HeatTransfer = true);
    AirEnergy_Kuehlung_circle2.Wand wand2(T_start(displayUnit = "K") = 293.15, area_h = 1.72e-4, c_wall = 888, dT(displayUnit = "K") = 0, k_wall = 220, rho_wall = 2700, s = 0.04);
    Modelica.Fluid.Interfaces.FluidPort_a port_a1 (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_a port_a2 (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_b port_b1 (replaceable package Medium = Medium);
    Modelica.Fluid.Interfaces.FluidPort_b port_b2 (replaceable package Medium = Medium);
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell2;
    AirEnergy_Kuehlung_circle2.CooledPouchCell cooledPouchCell3;
  equation
    connect(pipe1.port_b, port_b);
    connect(pipe1.port_a, port_a);
    connect(dynamicPipe.port_a, fluidPort_a);
    connect(dynamicPipe.port_b, fluidPort_b);
    connect(cooledPouchCell.Qpipe, dynamicPipe.heatPorts[1]);
    connect(dynamicPipe1.port_b, port_b1);
    connect(dynamicPipe2.port_b, port_b2);
    connect(dynamicPipe1.port_a, port_a1);
    connect(dynamicPipe2.port_a, port_a2);
    connect(wand1.heatPort_a[1], dynamicPipe.heatPorts[2]);
    connect(wand1.heatPort_b[1], dynamicPipe1.heatPorts[1]);
    connect(wand.heatPort_a[1], dynamicPipe1.heatPorts[2]);
    connect(wand.heatPort_b[1], dynamicPipe2.heatPorts[1]);
    connect(wand2.heatPort_a[1], dynamicPipe2.heatPorts[2]);
    connect(wand2.heatPort_b[1], pipe1.heatPorts[1]);
    connect(cooledPouchCell1.Qpipe, pipe1.heatPorts[2]);
    connect(cooledPouchCell2.Qpipe, dynamicPipe1.heatPorts[3]);
    connect(cooledPouchCell3.Qpipe, dynamicPipe2.heatPorts[3]);
  end PouchCellAndPipe;

  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe1;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe2;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe3;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe4;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe5;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe6;
  AirEnergy_Kuehlung_circle2.PouchCellAndPipe pouchCellAndPipe7;
  Modelica.Fluid.Sensors.TemperatureTwoPort KuehlmediumTempOut(replaceable package Medium = Medium);
  Modelica.Fluid.Sensors.TemperatureTwoPort KuehlmediumTempIn (replaceable package Medium = Medium);
  Modelica.Fluid.Sources.FixedBoundary boundary1(replaceable package Medium = Medium, T = 293.15, nPorts = 1);


equation
  connect(pouchCellAndPipe.fluidPort_b, pouchCellAndPipe1.fluidPort_a);
  connect(pouchCellAndPipe1.fluidPort_b, pouchCellAndPipe2.fluidPort_a);
  connect(pouchCellAndPipe1.port_a, pouchCellAndPipe.port_b);
  connect(pouchCellAndPipe2.port_a, pouchCellAndPipe1.port_b);
  connect(pouchCellAndPipe.port_a, KuehlmediumTempOut.port_b);
  connect(boundary.ports[1], KuehlmediumTempIn.port_a);
  connect(KuehlmediumTempIn.port_b, pouchCellAndPipe.fluidPort_a);
  connect(KuehlmediumTempOut.port_a, boundary1.ports[1]);
  connect(pouchCellAndPipe2.fluidPort_b, pouchCellAndPipe3.fluidPort_a);
  connect(pouchCellAndPipe3.fluidPort_b, pouchCellAndPipe4.fluidPort_a);
  connect(pouchCellAndPipe4.fluidPort_b, pouchCellAndPipe5.fluidPort_a);
  connect(pouchCellAndPipe5.fluidPort_b, pouchCellAndPipe6.fluidPort_a);
  connect(pouchCellAndPipe6.fluidPort_b, pouchCellAndPipe7.fluidPort_a);
  connect(pouchCellAndPipe7.fluidPort_b, pouchCellAndPipe7.port_b1);
  connect(pouchCellAndPipe7.port_a1, pouchCellAndPipe6.port_b1);
  connect(pouchCellAndPipe6.port_a1, pouchCellAndPipe5.port_b1);
  connect(pouchCellAndPipe5.port_a1, pouchCellAndPipe4.port_b1);
  connect(pouchCellAndPipe4.port_a1, pouchCellAndPipe3.port_b1);
  connect(pouchCellAndPipe3.port_a1, pouchCellAndPipe2.port_b1);
  connect(pouchCellAndPipe2.port_a1, pouchCellAndPipe1.port_b1);
  connect(pouchCellAndPipe1.port_a1, pouchCellAndPipe.port_b1);
  connect(pouchCellAndPipe.port_a1, pouchCellAndPipe.port_a2);
  connect(pouchCellAndPipe.port_b2, pouchCellAndPipe1.port_a2);
  connect(pouchCellAndPipe1.port_b2, pouchCellAndPipe2.port_a2);
  connect(pouchCellAndPipe2.port_b2, pouchCellAndPipe3.port_a2);
  connect(pouchCellAndPipe3.port_b2, pouchCellAndPipe4.port_a2);
  connect(pouchCellAndPipe4.port_b2, pouchCellAndPipe5.port_a2);
  connect(pouchCellAndPipe5.port_b2, pouchCellAndPipe6.port_a2);
  connect(pouchCellAndPipe6.port_b2, pouchCellAndPipe7.port_a2);
  connect(pouchCellAndPipe7.port_b2, pouchCellAndPipe7.port_b);
  connect(pouchCellAndPipe7.port_a, pouchCellAndPipe6.port_b);
  connect(pouchCellAndPipe6.port_a, pouchCellAndPipe5.port_b);
  connect(pouchCellAndPipe5.port_a, pouchCellAndPipe4.port_b);
  connect(pouchCellAndPipe4.port_a, pouchCellAndPipe3.port_b);
  connect(pouchCellAndPipe3.port_a, pouchCellAndPipe2.port_b);

end AirEnergy_Kuehlung_circle2;
