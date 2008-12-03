--  $Id: aperiodic_server_parameters.ads,v 1.3 2008/11/20 18:49:43 baker Exp $

with Virtual_Times; use Virtual_Times;
package Aperiodic_Server_Parameters is
   type Parameters is record
      Budget : Time;
      Budget_Interval : Time;
   end record;
end Aperiodic_Server_Parameters;
