with AWS.Response;
with AWS.Services.Page_Server;
with AWS.Services.Directory;
with AWS.Server;
with AWS.Status;

package Server is

   function Callback (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Start_Server;

private

end Server;