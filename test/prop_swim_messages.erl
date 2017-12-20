%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017. All Rights Reserved.
%%%
%%% Licensed under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file except in compliance
%%% with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitiations under the License.
%%% ----------------------------------------------------------------------------

-module(prop_swim_messages).

-include_lib("proper/include/proper.hrl").

-import(swim_generators, [swim_message/0]).

prop_encode_decode() ->
    ?FORALL({Type, _Events} = Msg, swim_message(),
            aggregate([element(1, Type)], begin
                Data = iolist_to_binary(swim_messages:encode(Msg)),
                Msg =:= swim_messages:decode(Data)
            end)).
