
module S = Set.Make(Uchar)
let ints xs = S.of_list (List.map Uchar.of_int xs)

(* RFC5892 Section 2.1 LetterDigits (A)
   A: General_Category(cp) is in {Ll, Lu, Lo, Nd, Lm, Mn, Mc} *)
let a cp =
  match Uucp.Gc.general_category cp with
  | `Ll | `Lu | `Lo | `Nd | `Lm | `Mn | `Mc -> true
  | _ -> false

(* RFC5892 Section 2.6 Exceptions (F)
   F: cp is in {00B7, 00DF, 0375, 03C2, 05F3, 05F4, 0640, 0660,
                0661, 0662, 0663, 0664, 0665, 0666, 0667, 0668,
                0669, 06F0, 06F1, 06F2, 06F3, 06F4, 06F5, 06F6,
                06F7, 06F8, 06F9, 06FD, 06FE, 07FA, 0F0B, 3007,
                302E, 302F, 3031, 3032, 3033, 3034, 3035, 303B,
                30FB} *)
let f_pvalid =
  (* PVALID -- Would otherwise have been DISALLOWED
     00DF; PVALID     # LATIN SMALL LETTER SHARP S
     03C2; PVALID     # GREEK SMALL LETTER FINAL SIGMA
     06FD; PVALID     # ARABIC SIGN SINDHI AMPERSAND
     06FE; PVALID     # ARABIC SIGN SINDHI POSTPOSITION MEN
     0F0B; PVALID     # TIBETAN MARK INTERSYLLABIC TSHEG
     3007; PVALID     # IDEOGRAPHIC NUMBER ZERO *)
  ints [ 0x00DF; 0x03C2; 0x06FD; 0x06FE; 0x0F0B; 0x3007 ]

and f_contexto =
  (* CONTEXTO -- Would otherwise have been DISALLOWED
     00B7; CONTEXTO   # MIDDLE DOT
     0375; CONTEXTO   # GREEK LOWER NUMERAL SIGN (KERAIA)
     05F3; CONTEXTO   # HEBREW PUNCTUATION GERESH
     05F4; CONTEXTO   # HEBREW PUNCTUATION GERSHAYIM
     30FB; CONTEXTO   # KATAKANA MIDDLE DOT *)
  S.union
    (ints [ 0x00B7; 0x0375; 0x05F3; 0x05F4; 0x30FB ])
  (* CONTEXTO -- Would otherwise have been PVALID
     0660; CONTEXTO   # ARABIC-INDIC DIGIT ZERO
     0661; CONTEXTO   # ARABIC-INDIC DIGIT ONE
     0662; CONTEXTO   # ARABIC-INDIC DIGIT TWO
     0663; CONTEXTO   # ARABIC-INDIC DIGIT THREE
     0664; CONTEXTO   # ARABIC-INDIC DIGIT FOUR
     0665; CONTEXTO   # ARABIC-INDIC DIGIT FIVE
     0666; CONTEXTO   # ARABIC-INDIC DIGIT SIX
     0667; CONTEXTO   # ARABIC-INDIC DIGIT SEVEN
     0668; CONTEXTO   # ARABIC-INDIC DIGIT EIGHT
     0669; CONTEXTO   # ARABIC-INDIC DIGIT NINE
     06F0; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT ZERO
     06F1; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT ONE
     06F2; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT TWO
     06F3; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT THREE
     06F4; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT FOUR
     06F5; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT FIVE
     06F6; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT SIX
     06F7; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT SEVEN
     06F8; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT EIGHT
     06F9; CONTEXTO   # EXTENDED ARABIC-INDIC DIGIT NINE *)
    (ints [
        0x0660; 0x0661; 0x0662; 0x0663; 0x0664; 0x0665; 0x0666; 0x0667; 0x0668;
        0x0669;
        0x06F0; 0x06F1; 0x06F2; 0x06F3; 0x06F4; 0x06F5; 0x06F6; 0x06F7; 0x06F8;
        0x06F9
      ])

and f_disallowed =
(* DISALLOWED -- Would otherwise have been PVALID
   0640; DISALLOWED # ARABIC TATWEEL
   07FA; DISALLOWED # NKO LAJANYALAN
   302E; DISALLOWED # HANGUL SINGLE DOT TONE MARK
   302F; DISALLOWED # HANGUL DOUBLE DOT TONE MARK
   3031; DISALLOWED # VERTICAL KANA REPEAT MARK
   3032; DISALLOWED # VERTICAL KANA REPEAT WITH VOICED SOUND MARK
   3033; DISALLOWED # VERTICAL KANA REPEAT MARK UPPER HALF
   3034; DISALLOWED # VERTICAL KANA REPEAT WITH VOICED SOUND MARK UPPER HA
   3035; DISALLOWED # VERTICAL KANA REPEAT MARK LOWER HALF
   303B; DISALLOWED # VERTICAL IDEOGRAPHIC ITERATION MARK *)
  ints [ 0x0640; 0x07FA; 0x302E; 0x302F; 0x3031; 0x3032; 0x3033; 0x3034; 0x3035;
         0x303B ]

let f cp =
  S.mem cp f_pvalid || S.mem cp f_contexto || S.mem cp f_disallowed

let f_class cp =
  if S.mem cp f_pvalid then `Protocol_valid
  else if S.mem cp f_contexto then `Context_other
  else (assert (S.mem cp f_disallowed) ; `Disallowed)

let h cp =
  (* RFC 5892 Section 2.8 JoinControl (H)
     H: Join_Control(cp) = True *)
  Uucp.Func.is_join_control cp

let i cp =
  (* RFC 5892 Section 2.9 OldHangulJamo (I)
     I: Hangul_Syllable_Type(cp) is in {L, V, T} *)
  match Uucp.Hangul.syllable_type cp with
  | `L | `T | `V -> true
  | _ -> false

let k cp =
  (* RFC 8264 Section 9.11.  ASCII7 (K)
     K: cp is in {0021..007E} *)
  let int = Uchar.to_int cp in
  int >= 0x21 && int <= 0x7E

let l cp =
  (* RFC 8264 Section 9.12.  Controls (L)
     L: Control(cp) = True *)
  Uucp.Gc.general_category cp = `Cc

let m cp =
  (* RFC 8264 Section  9.13.  PrecisIgnorableProperties (M)

   This PRECIS-specific category is used to group code points that are
   discouraged from use in PRECIS string classes.

   M: Default_Ignorable_Code_Point(cp) = True or
      Noncharacter_Code_Point(cp) = True *)
  Uucp.Gen.is_Default_ignorable cp || Uucfp.Gen.is_non_character

let n cp =
  (* RFC 8264 Section 9.14.  Spaces (N)
     N: General_Category(cp) is in {Zs} *)
  Uucp.Gc.general_category cp = `Zs

let o cp =
  (* RFC 8264 Section 9.15.  Symbols (O)
     O: General_Category(cp) is in {Sm, Sc, Sk, So} *)
  match Uucp.Gc.general_category cp with
  | `Sm | `Sc | `Sk | `So -> true
  | _ -> false

let p cp =
  (* RFC 8264 Section 9.16.  Punctuation (P)
     P: General_Category(cp) is in {Pc, Pd, Ps, Pe, Pi, Pf, Po} *)
  match Uucp.Gc.general_category cp with
  | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po -> true
  | _ -> false

let q cp =
  (* RFC 8264 Section 9.17.  HasCompat (Q)
     Q: toNFKC(cp) != cp *)
  match Uucp.Case.Nfkc_fold.fold cp with
  | `Self -> false
  | `Uchars _ -> true

let r cp =
  (* RFC 8264 Section 9.18.  OtherLetterDigits (R)
     R: General_Category(cp) is in {Lt, Nl, No, Me} *)
  match Uucp.Gc.general_category cp with
  | `Lt | `Nl | `No | `Me -> true
  | _ -> false
