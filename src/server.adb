with Ada.Text_IO; use Ada.Text_IO;
with CLIC.TTY;
with AAA.Strings;

package body Server is

   package TT renames CLIC.TTY;

   Server_Port : constant Positive := 8_888;
   Http_Server : AWS.Server.HTTP;

   function Callback (Request : AWS.Status.Data) return AWS.Response.Data is
      use AWS.Services.Directory;

      Uri : constant String := AWS.Status.URI (D => Request);
   begin
      return AWS.Services.Page_Server.Callback (Request => Request);

   end Callback;

   procedure Start_Server is
      Link : constant String :=
      "http://localhost:" &
      Positive'Image (Server_Port)
      (Positive'Image (Server_Port)'First + 1
      .. Positive'Image (Server_Port)'Length) &
      "/index.html";
   begin
      AWS.Server.Start
        (Web_Server     => Http_Server,
         Name           => "WebsiteGenerator",
         Port           => Server_Port,
         Callback       => Callback'Access,
         Max_Connection => 5);

      Put_Line
      (Item => "Server was started. Press ""Control-C"" to quit.");

      Put_Line (TT.URL (Link));

   end Start_Server;

end Server;
