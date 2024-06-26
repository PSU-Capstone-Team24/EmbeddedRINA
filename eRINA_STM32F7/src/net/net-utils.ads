-----------------------------------------------------------------------
--  net-utils -- Network utilities
--  Copyright (C) 2016, 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
package Net.Utils is

   pragma Preelaborate;

   --  Convert the IPv4 address to a dot string representation.
   function To_String (Ip : in Ip_Addr) return String;

   --  Convert the Ethernet address to a string representation.
   function To_String (Mac : in Ether_Addr) return String;

   function Hex (Value : in Uint8) return String;
end Net.Utils;
