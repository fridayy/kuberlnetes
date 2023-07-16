searchNodes=[{"doc":"kuberlnetes A low level kubernetes api helper - primarlily for applications that interact with the kubernetes api from inside the cluster.","ref":"kuberlnetes.html","title":"kuberlnetes","type":"module"},{"doc":"Deletes the given resource","ref":"kuberlnetes.html#delete/2","title":"kuberlnetes.delete/2","type":"function"},{"doc":"","ref":"kuberlnetes.html#from_config/0","title":"kuberlnetes.from_config/0","type":"function"},{"doc":"Loads the current-context from ~/.kube/config","ref":"kuberlnetes.html#from_config/1","title":"kuberlnetes.from_config/1","type":"function"},{"doc":"","ref":"kuberlnetes.html#from_raw/1","title":"kuberlnetes.from_raw/1","type":"function"},{"doc":"","ref":"kuberlnetes.html#get/1","title":"kuberlnetes.get/1","type":"function"},{"doc":"Initiates a GET request against the configured kubernetes api server or raises an error if the server is not configured correctly.","ref":"kuberlnetes.html#get/2","title":"kuberlnetes.get/2","type":"function"},{"doc":"","ref":"kuberlnetes.html#headers/1","title":"kuberlnetes.headers/1","type":"function"},{"doc":"","ref":"kuberlnetes.html#headers/2","title":"kuberlnetes.headers/2","type":"function"},{"doc":"Configures a server using the service account","ref":"kuberlnetes.html#in_cluster/0","title":"kuberlnetes.in_cluster/0","type":"function"},{"doc":"Returns the current datetime in the kubernetes MicroTime format see: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.22/#microtime-v1-meta","ref":"kuberlnetes.html#microtime_now/0","title":"kuberlnetes.microtime_now/0","type":"function"},{"doc":"Initiates a PATCH request against the configured kubernetes api server or raises an error if the server is not configured correctly. see: server()","ref":"kuberlnetes.html#patch/2","title":"kuberlnetes.patch/2","type":"function"},{"doc":"Initiates a POST request against the configured kubernetes api server or raises an error if the server is not configured correctly. see: server()","ref":"kuberlnetes.html#post/2","title":"kuberlnetes.post/2","type":"function"},{"doc":"","ref":"kuberlnetes.html#watch/1","title":"kuberlnetes.watch/1","type":"function"},{"doc":"Closes an active watch process when not running in a supervision tree.","ref":"kuberlnetes.html#watch_close/1","title":"kuberlnetes.watch_close/1","type":"function"},{"doc":"Returns all currently active watch processes","ref":"kuberlnetes.html#watches/0","title":"kuberlnetes.watches/0","type":"function"},{"doc":"","ref":"kuberlnetes.html#t:http_status_err/0","title":"kuberlnetes.http_status_err/0","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:kube_cfg_options/0","title":"kuberlnetes.kube_cfg_options/0","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:maybe/1","title":"kuberlnetes.maybe/1","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:mutation_request/0","title":"kuberlnetes.mutation_request/0","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:options/0","title":"kuberlnetes.options/0","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:resp_with_body/0","title":"kuberlnetes.resp_with_body/0","type":"type"},{"doc":"","ref":"kuberlnetes.html#t:server/0","title":"kuberlnetes.server/0","type":"type"},{"doc":"kuberlnetes_app","ref":"kuberlnetes_app.html","title":"kuberlnetes_app","type":"module"},{"doc":"","ref":"kuberlnetes_app.html#start/2","title":"kuberlnetes_app.start/2","type":"function"},{"doc":"","ref":"kuberlnetes_app.html#stop/1","title":"kuberlnetes_app.stop/1","type":"function"},{"doc":"kuberlnetes_sup","ref":"kuberlnetes_sup.html","title":"kuberlnetes_sup","type":"module"},{"doc":"","ref":"kuberlnetes_sup.html#init/1","title":"kuberlnetes_sup.init/1","type":"function"},{"doc":"","ref":"kuberlnetes_sup.html#start_link/0","title":"kuberlnetes_sup.start_link/0","type":"function"},{"doc":"gen_server process providing kubernetes watch functionality. Supervised by kuberlnetes_watch_sup - each process represents an open watch for a given resource.","ref":"kuberlnetes_watch.html","title":"kuberlnetes_watch","type":"module"},{"doc":"","ref":"kuberlnetes_watch.html#handle_call/3","title":"kuberlnetes_watch.handle_call/3","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#handle_cast/2","title":"kuberlnetes_watch.handle_cast/2","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#handle_continue/2","title":"kuberlnetes_watch.handle_continue/2","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#handle_info/2","title":"kuberlnetes_watch.handle_info/2","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#init/1","title":"kuberlnetes_watch.init/1","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#start_link/3","title":"kuberlnetes_watch.start_link/3","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#stop/1","title":"kuberlnetes_watch.stop/1","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#terminate/2","title":"kuberlnetes_watch.terminate/2","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#watch/2","title":"kuberlnetes_watch.watch/2","type":"function"},{"doc":"","ref":"kuberlnetes_watch.html#t:supported_kind/0","title":"kuberlnetes_watch.supported_kind/0","type":"type"},{"doc":"","ref":"kuberlnetes_watch.html#t:watch_opts/0","title":"kuberlnetes_watch.watch_opts/0","type":"type"},{"doc":"","ref":"kuberlnetes_watch_sup.html","title":"kuberlnetes_watch_sup","type":"module"},{"doc":"","ref":"kuberlnetes_watch_sup.html#init/1","title":"kuberlnetes_watch_sup.init/1","type":"function"},{"doc":"","ref":"kuberlnetes_watch_sup.html#start_link/0","title":"kuberlnetes_watch_sup.start_link/0","type":"function"},{"doc":"An rather low-level OTP library for interacting with the Kubernetes API server. Primarly used in applications that interact with Kubernetes from within the cluster.","ref":"readme.html","title":"kuberlnetes","type":"extras"},{"doc":"The primary goal kuberlnetes is not to create a high-level abstraction over the Kubernetes API but provide simple functions that simplify certain operations and make them easily available for Erlang applications. There is no generated code or any forced model - only supporting functions for the well-known REST operations. This eases backward and forward compatibility (at least for kuberlnetes itself).","ref":"readme.html#why","title":"kuberlnetes - Why?","type":"extras"},{"doc":"kuberlnetes returns the plain responses as defined in the Kubernetes API specification as Erlang maps. Bear in mind that get | post | patch | put functions are blocking! Applications accessing the Kubernetes API from within the cluster: Server = kuberlnetes : in_cluster ( ) , { ok , DeploymentsList } = kuberlnetes : get ( &quot;/apis/apps/v1/deployments&quot; ) , \#{ &lt;&lt; &quot;items&quot; &gt;&gt; := Deployments } = DeploymentsList , . . . Access the Kubernetes API from outside the cluster using a local ~/.kube/config file: Server = kuberlnetes : from_config ( \#{ context =&gt; &quot; some_context } } ) , . . .","ref":"readme.html#basic-examples","title":"kuberlnetes - Basic Examples","type":"extras"},{"doc":"kuberlnetes supports the Watch API : Server = kuberlnetes : from_config ( \#{ context =&gt; &quot;some_context&quot; } ) , { ok , WatchPid } = kuberlnetes : watch ( \#{ server =&gt; Server , api_group =&gt; &quot;/api/v1&quot; , kind =&gt; &quot;configmaps&quot; , name =&gt; &quot;my-config&quot; , namespace =&gt; &quot;default&quot; } ) . Each open watch is represented by an erlang process. By default messages change messages are sent to the process that spawned the watch proce The received message has the following format: { kuberlnetes_watch , added , \#{ kind =&gt; &lt;&lt; &quot;Pod&quot; &gt;&gt; , . . . } } | { kuberlnetes_watch , modified , \#{ . . . } } | { kuberlnetes_watch , deleted , \#{ . . . } } .","ref":"readme.html#watches","title":"kuberlnetes - Watches","type":"extras"},{"doc":"$ make","ref":"readme.html#build","title":"kuberlnetes - Build","type":"extras"},{"doc":"Apache License Version 2.0, January 2004 http://www.apache.org/licenses/ TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION 1. Definitions. &quot;License&quot; shall mean the terms and conditions for use, reproduction, and distribution as defined by Sections 1 through 9 of this document. &quot;Licensor&quot; shall mean the copyright owner or entity authorized by the copyright owner that is granting the License. &quot;Legal Entity&quot; shall mean the union of the acting entity and all other entities that control, are controlled by, or are under common control with that entity. For the purposes of this definition, &quot;control&quot; means (i) the power, direct or indirect, to cause the direction or management of such entity, whether by contract or otherwise, or (ii) ownership of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ownership of such entity. &quot;You&quot; (or &quot;Your&quot;) shall mean an individual or Legal Entity exercising permissions granted by this License. &quot;Source&quot; form shall mean the preferred form for making modifications, including but not limited to software source code, documentation source, and configuration files. &quot;Object&quot; form shall mean any form resulting from mechanical transformation or translation of a Source form, including but not limited to compiled object code, generated documentation, and conversions to other media types. &quot;Work&quot; shall mean the work of authorship, whether in Source or Object form, made available under the License, as indicated by a copyright notice that is included in or attached to the work (an example is provided in the Appendix below). &quot;Derivative Works&quot; shall mean any work, whether in Source or Object form, that is based on (or derived from) the Work and for which the editorial revisions, annotations, elaborations, or other modifications represent, as a whole, an original work of authorship. For the purposes of this License, Derivative Works shall not include works that remain separable from, or merely link (or bind by name) to the interfaces of, the Work and Derivative Works thereof. &quot;Contribution&quot; shall mean any work of authorship, including the original version of the Work and any modifications or additions to that Work or Derivative Works thereof, that is intentionally submitted to Licensor for inclusion in the Work by the copyright owner or by an individual or Legal Entity authorized to submit on behalf of the copyright owner. For the purposes of this definition, &quot;submitted&quot; means any form of electronic, verbal, or written communication sent to the Licensor or its representatives, including but not limited to communication on electronic mailing lists, source code control systems, and issue tracking systems that are managed by, or on behalf of, the Licensor for the purpose of discussing and improving the Work, but excluding communication that is conspicuously marked or otherwise designated in writing by the copyright owner as &quot;Not a Contribution.&quot; &quot;Contributor&quot; shall mean Licensor and any individual or Legal Entity on behalf of whom a Contribution has been received by Licensor and subsequently incorporated within the Work. 2. Grant of Copyright License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable copyright license to reproduce, prepare Derivative Works of, publicly display, publicly perform, sublicense, and distribute the Work and such Derivative Works in Source or Object form. 3. Grant of Patent License. Subject to the terms and conditions of this License, each Contributor hereby grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable (except as stated in this section) patent license to make, have made, use, offer to sell, sell, import, and otherwise transfer the Work, where such license applies only to those patent claims licensable by such Contributor that are necessarily infringed by their Contribution(s) alone or by combination of their Contribution(s) with the Work to which such Contribution(s) was submitted. If You institute patent litigation against any entity (including a cross-claim or counterclaim in a lawsuit) alleging that the Work or a Contribution incorporated within the Work constitutes direct or contributory patent infringement, then any patent licenses granted to You under this License for that Work shall terminate as of the date such litigation is filed. 4. Redistribution. You may reproduce and distribute copies of the Work or Derivative Works thereof in any medium, with or without modifications, and in Source or Object form, provided that You meet the following conditions: (a) You must give any other recipients of the Work or Derivative Works a copy of this License; and (b) You must cause any modified files to carry prominent notices stating that You changed the files; and (c) You must retain, in the Source form of any Derivative Works that You distribute, all copyright, patent, trademark, and attribution notices from the Source form of the Work, excluding those notices that do not pertain to any part of the Derivative Works; and (d) If the Work includes a &quot;NOTICE&quot; text file as part of its distribution, then any Derivative Works that You distribute must include a readable copy of the attribution notices contained within such NOTICE file, excluding those notices that do not pertain to any part of the Derivative Works, in at least one of the following places: within a NOTICE text file distributed as part of the Derivative Works; within the Source form or documentation, if provided along with the Derivative Works; or, within a display generated by the Derivative Works, if and wherever such third-party notices normally appear. The contents of the NOTICE file are for informational purposes only and do not modify the License. You may add Your own attribution notices within Derivative Works that You distribute, alongside or as an addendum to the NOTICE text from the Work, provided that such additional attribution notices cannot be construed as modifying the License. You may add Your own copyright statement to Your modifications and may provide additional or different license terms and conditions for use, reproduction, or distribution of Your modifications, or for any such Derivative Works as a whole, provided Your use, reproduction, and distribution of the Work otherwise complies with the conditions stated in this License. 5. Submission of Contributions. Unless You explicitly state otherwise, any Contribution intentionally submitted for inclusion in the Work by You to the Licensor shall be under the terms and conditions of this License, without any additional terms or conditions. Notwithstanding the above, nothing herein shall supersede or modify the terms of any separate license agreement you may have executed with Licensor regarding such Contributions. 6. Trademarks. This License does not grant permission to use the trade names, trademarks, service marks, or product names of the Licensor, except as required for reasonable and customary use in describing the origin of the Work and reproducing the content of the NOTICE file. 7. Disclaimer of Warranty. Unless required by applicable law or agreed to in writing, Licensor provides the Work (and each Contributor provides its Contributions) on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied, including, without limitation, any warranties or conditions of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You are solely responsible for determining the appropriateness of using or redistributing the Work and assume any risks associated with Your exercise of permissions under this License. 8. Limitation of Liability. In no event and under no legal theory, whether in tort (including negligence), contract, or otherwise, unless required by applicable law (such as deliberate and grossly negligent acts) or agreed to in writing, shall any Contributor be liable to You for damages, including any direct, indirect, special, incidental, or consequential damages of any character arising as a result of this License or out of the use or inability to use the Work (including but not limited to damages for loss of goodwill, work stoppage, computer failure or malfunction, or any and all other commercial damages or losses), even if such Contributor has been advised of the possibility of such damages. 9. Accepting Warranty or Additional Liability. While redistributing the Work or Derivative Works thereof, You may choose to offer, and charge a fee for, acceptance of support, warranty, indemnity, or other liability obligations and/or rights consistent with this License. However, in accepting such obligations, You may act only on Your own behalf and on Your sole responsibility, not on behalf of any other Contributor, and only if You agree to indemnify, defend, and hold each Contributor harmless for any liability incurred by, or claims asserted against, such Contributor by reason of your accepting any such warranty or additional liability. END OF TERMS AND CONDITIONS Copyright 2022, fridayy# &lt;benjamin.krenn@leftshift.one&gt;. Licensed under the Apache License, Version 2.0 (the &quot;License&quot;); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.","ref":"license.html","title":"LICENSE","type":"extras"}]