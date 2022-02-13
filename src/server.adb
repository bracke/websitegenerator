with AWS.Response;
with AWS.Services.Page_Server;
with AWS.Services.Directory;
with AWS.Server;
with AWS.Status;
with Ada.Text_IO; use Ada.Text_IO;

package body Server is

   Server_Port : Positive := 8_888;
   Http_Server : AWS.Server.HTTP;

  function Callback(Request: AWS.Status.Data) return AWS.Response.Data is
      use AWS.Services.Directory;

      Uri: constant String := AWS.Status.URI(D => Request);
   begin
      return AWS.Services.Page_Server.Callback (Request => Request);

   end Callback;

   procedure Start_Server is
   begin
      AWS.Server.Start
        (Web_Server     => Http_Server, Name => "WebsiteGenerator",
         Port           => Server_Port, Callback => Callback'Access,
         Max_Connection => 5);
      Put_Line
        (Item =>
           "Server was started. Web address: http://localhost:" &
           Positive'Image (Server_Port)
             (Positive'Image (Server_Port)'First + 1 ..
                  Positive'Image (Server_Port)'Length) &
           "/index.html Press ""Control-C"" for quit.");
   end Start_Server;

end Server;
