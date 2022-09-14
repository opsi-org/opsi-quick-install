unit opsi_quick_install_nogui_NetworkSuggestions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  osnetworkcalculator;

function GetNetmaskSuggestions(NetworkDetails: array of string): string;
function GetNetworkAddressSuggestions(NetworkDetails: array of string): string;
function GetDomainSuggestions(NetworkDetails: array of string): string;
function GetNameserverSuggestions(NetworkDetails: array of string): string;

implementation

function GetNetmaskSuggestions(NetworkDetails: array of string): string;
var
  Suggestions: string = '';
  network: array of string;
  index: integer = 0;
begin
  // IP4.ADDRESS[1]
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    Suggestions += getNetmaskByIP4adr(network[1]);
    // IP4.ADDRESS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      Suggestions += ', ' + getNetmaskByIP4adr(network[1]);
      // IP4.ADDRESS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        Suggestions += ', ' + getNetmaskByIP4adr(network[1]);
      end;
    end;
  end;
  Result := Suggestions;
end;

function GetNetworkAddressSuggestions(NetworkDetails: array of string): string;
var
  Suggestions: string = '';
  network: array of string;
  index: integer = 0;
begin
  // IP4.ADDRESS[1]
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    Suggestions += getIP4NetworkByAdrAndMask(network[0], network[1]);
    // IP4.ADDRESS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      Suggestions += ', ' + getIP4NetworkByAdrAndMask(network[0], network[1]);
      // IP4.ADDRESS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        Suggestions += ', ' + getIP4NetworkByAdrAndMask(network[0], network[1]);
      end;
    end;
  end;
  Result := Suggestions;
end;

function GetDomainSuggestions(NetworkDetails: array of string): string;
var
  Suggestions: string = '';
  index: integer = 3;
begin
  // IP4.DOMAIN[1]
  if NetworkDetails[index] <> '' then
  begin
    Suggestions += NetworkDetails[index];
    // IP4.DOMAIN[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      Suggestions += ', ' + NetworkDetails[index];
      // IP4.DOMAIN[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
        Suggestions += ', ' + NetworkDetails[index];
    end;
  end;
  Result := Suggestions;
end;

function GetNameserverSuggestions(NetworkDetails: array of string): string;
var
  Suggestions: string = '';
  index: integer = 6;
begin
  // IP4.DNS[1]
  if NetworkDetails[index] <> '' then
  begin
    Suggestions += NetworkDetails[index];
    // IP4.DNS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      Suggestions += ', ' + NetworkDetails[index];
      // IP4.DNS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
        Suggestions += ', ' + NetworkDetails[index];
    end;
  end;
  Result := Suggestions;
end;


end.
