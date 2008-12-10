-- $Id: sizes.ads,v 1.2 2008/11/24 02:05:39 baker Exp $

--  configurable values that define a set of experiments to run

with Periodic_Workloads;
with Aperiodic_Workloads;
with Virtual_Times; use Virtual_Times;
--  with Ada.Text_IO; use Ada.Text_IO;
--- with Random_Tools; use Random_Tools;

package Sizes is

  subtype Periodic is Periodic_Workloads.Parameters;
  subtype Aperiodic is Aperiodic_Workloads.Parameters;

  type Int_Array is array (Integer range <>) of Integer;
  type Float_Array is array (Integer range <>) of float;

  --  period of the aperiodic server

  Server_Period:  constant:=  5400;

  --  number of different average aperiodic inter-arrival times we
  --  want to test

  subtype IATS is integer range 1..3;

  --  set of values to try for average inter-arrival time
  --  of jobs for aperiodic server

  Interarrival_Times : constant Float_Array (IATS):=
     (1800.0, 3600.0, 5400.0);

  --  number of different aperiodic loads we want to test

  subtype Loads is integer range 1..11;

  --  number of different periodic task sets we want to test

  subtype Runs is integer range 1..3;

  --  set of load levels for the aperiodic

  type Float_Table is array (Runs) of Float_Array (Loads);

  Aperiodic_Load: constant Float_Table:=
  ((0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55),
   (0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275),
   (0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.10, 0.11));


  --  total utilization (U_sum) of each periodic set

  U: constant Int_Array (Runs) := ( 40, 69, 88);

  --  number of periodic tasks per set

  subtype Periodic_Range is Integer range 1 .. 10;

  --  periodic task sets to run
  --  in competition with the aperiodic server

  type Periodic_List is array (Periodic_Range) of Periodic;
  type Periodic_Set is array (Runs) of Periodic_List;

  Periodic_Parameters : constant Periodic_Set :=
  -- (period, deadline, WCET)
    ( -- 40% utilization
     ((   5400,   5400,   200),
      (  10800,  10800,   600),
      (  21600,  21600,  1600),
      (  27000,  27000,  3000),
      (  36000,  36000,   400),
      (  43200,  43200,  1200),
      (  54000,  54000,  1000),
      (  67500,  67500,  1500),
      ( 108000, 108000,  1000),
      ( 120000, 120000,  4000)),
      --  69%  utilization
     ((   5400,   5400,   600),
      (  14400,  14400,   600),
      (  24000,  24000,   500),
      (  43200,  43200,  6400),
      (  54000,  54000,  3000),
      (  67500,  67500,  4500),
      (  72000,  72000,  7200),
      (  90000,  90000,  3600),
      ( 108000, 108000,  2000),
      ( 120000, 120000, 10500)),
      --  88% utilization
     ((   5400,   5400,   300),
      (  10800,  10800,  1000),
      (  21600,  21600,  2800),
      (  30000,  30000,  1400),
      (  43200,  43200,  7200),
      (  54000,  54000, 11000),
      (  60000,  60000,  3600),
      (  90000,  90000,  6600),
      ( 108000, 108000,  2000),
      ( 120000, 120000,  4000)));

  --  list of aperiodic server algorithms

  -- Order is very important

  type Servers is (BGS, PLS, DSS, TBS, DDS, DXS, CUS, BIS);
--  type Servers is (BGS);
  --  BGS = Background Server
  --  PLS = Polling Server
  --  DSS = Sporadic Server
  --  TBS = Total Bandwidth Server
  --  DDS = Deadline Deferrable Server
  --  DXS = Exchange Server
  --  CUS = Constant Utilization Server
  --  BIS = Baker Ideal Server
  --  We abuse these names for both deadline and fixed-task-priority
  --  variants of the algorithms, in different applications.

  Type Server_Size_List is array (Servers) of Time;
  type Server_Size_Table is array (Runs) of Server_Size_List;

  --  Maximum server budgets for Rate Monotonic versions
  --  of servers, based on utliization bound.
  --  Budgets for DDS and DXS are lower, because the
  --  analyses for these are different.

  RM_Size: constant Server_Size_Table:=
  ((DDS => 3181, DXS => 3093, others => 3160),
   (DDS => 1081, DXS => 1085, others => 1109),
   (DDS =>  117, DXS =>  117, others => 125));


  --  Maximum server budget for deadline-bases versions
  --  of servers.

  EDF_Size: constant Server_Size_Table:=
  ((DDS => 3181, others => 3240),
   (DDS => 1622, others => 1674),
   (DDS => 623, others => 648));

  --  Default number of times to repeat the experiments.

  Repeat: Integer := 1;

end Sizes;

