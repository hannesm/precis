open Precis_categories

module Identifier = struct
  let valid cp = a cp || k cp

  let contextual_rule_required cp = f cp || h cp

  let disallowed cp = i cp || l cp || m cp || n cp || o cp || p cp || q cp || r cp

  (* let unassigned = treat as disallowed *)
end

module Freeform = struct

  let valid cp = a cp || k cp || n cp || o cp || p cp || q cp || r cp

  let contextual_rule_required cp = f cp || h cp

  let disallowed cp = i cp || l cp || m cp

  (* let unassigned = treat as disallowed *)
end

type derived = [
  | `Protocol_valid
  (* | `Identifier_protocol_valid is not used, equals `Protocol_valid *)
  (* | `Freeform_protocol_valid is the same as `Identifier_disallowed *)
  | `Context_join
  | `Context_other
  | `Disallowed
  | `Identifier_disallowed
  (* | `Freeform_disallowed is not used, equals `Disallowed *)
  | `Unassigned
]

(* RFC 8264 Section 8 *)
let derived_prop cp =
  if f cp then f_class cp
  (* else if backwardscompatible *)
  else if Uucp.Age.age cp = `Unassigned then `Unassigned
  else if k cp then `Protocol_valid
  else if h cp then `Context_join
  else if i cp then `Disallowed
  else if m cp then `Disallowed
  else if l cp then `Disallowed
  else if q cp then `Identifier_disallowed
  else if a cp then `Protocol_valid
  else if r cp then `Identifier_disallowed
  else if n cp then `Identifier_disallowed
  else if o cp then `Identifier_disallowed
  else if p cp then `Identifier_disallowed
  else `Disallowed
