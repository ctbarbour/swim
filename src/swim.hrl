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

-type member()            :: {inet:ip_address(), inet:port_number()}.
-type member_status()     :: alive | suspect | faulty.
-type incarnation()       :: non_neg_integer().
-type sequence()          :: non_neg_integer().
-type user_event()        :: {user, term()}.
-type membership_event()  :: {membership, {member_status(), member(), incarnation()}}.
-type swim_event()        :: user_event() | membership_event().
-type swim_message()      :: {ack, sequence(), member(), [swim_event()]} |
                             {ping, sequence(), [swim_event()]} |
                             {ping_req, sequence(), member()} |
                             {leave, sequence()}.
-type key()               :: <<_:256>>.
-type event_category()    :: user | membership.
