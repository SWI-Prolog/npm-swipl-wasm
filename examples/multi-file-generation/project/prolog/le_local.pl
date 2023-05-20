/* le_local: a prolog module for LE handling of a local filesystem.

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*/

:- module(le_local, 
    [load_file_module/3, 
     this_capsule/1,
     portray_clause_ind/1,
     update_file/3,
     myDeclaredModule/1
    ]).

load_file_module(FileName, FileName, _) :-
   load_files([FileName], [module(FileName)]). 

this_capsule(user).
   %thread_self(M). % current_module(M) messes it up

portray_clause_ind(Clause) :- 
    portray_clause(Clause). 

:- multifile kp_loader:myDeclaredModule/1.

myDeclaredModule(user). 

update_file(NewFileName, _, String) :-
   open(NewFileName, write, Stream, []),
   write(Stream, String),
   close(Stream).