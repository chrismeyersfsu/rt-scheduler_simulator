--  $Id: job_queues.ads,v 1.2 2008/11/20 18:49:43 baker Exp $

with Jobs; use Jobs;
with Generic_Queues;
package Job_Queues is new Generic_Queues (Jobs.Job);
