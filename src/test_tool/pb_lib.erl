-module(pb_lib).
-description("pb_lib").
-author("arthorn").
-vsn(1.0).

%%%=======================EXPORT=======================
-compile(export_all).

%%%=======================INCLUDE======================

%%%=======================DEFINE======================

encode(Mod, Record) ->
    Mod:encode_msg(Record).

decode(Mod, Bin, MsgName) ->
    Mod:decode_msg(Bin, MsgName).



