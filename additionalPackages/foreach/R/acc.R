#
# Copyright (c) Microsoft. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

makeAccum <- function(it) {
  # define and return the accumulator function that will be
  # passed to eachElem
  function(results, tags) {
    if (identical(it$error.handling, 'stop') && !is.null(it$state$errorValue))
      return(invisible(NULL))

    for (i in seq(along.with=tags)) {
      if (it$verbose)
        cat(sprintf('got results for task %d\n', tags[i]))
      accumulate(it, results[[i]], tags[i])
    }
  }
}
