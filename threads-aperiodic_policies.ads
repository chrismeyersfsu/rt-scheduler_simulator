--  $Id: threads-aperiodic_policies.ads,v 1.1 2008/11/23 21:33:20 baker Exp $

-- base class for all aperiodic server policies

with Aperiodic_Server_Parameters; use Aperiodic_Server_Parameters;
package Threads.Aperiodic_Policies is

   type Object is abstract new
     Threads.Scheduling_Policies.Object with null record;
   
   type Class_Ref is access all Object'Class;

   procedure Bind_Parms
     (P : in out Object;
      Parms : Aperiodic_Server_Parameters.Parameters) is abstract;

end Threads.Aperiodic_Policies;
