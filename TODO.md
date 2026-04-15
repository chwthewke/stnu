# TODO

## meta

- move this TODO to a GH project at some point

## build

### plugins
- ~~scalac args~~
- ~~dependencies (smth fancy for named deps usable from sjs & scala-only? - or just always using %%% works?)~~
- ~~buildinfo~~

### core
- ~~with sjs crossbuild~~

### frontend
- ~~sjs bundler?~~ not for now
- ~~Tyrian~~
- ~~run it soon™~~
- ~~figure out flags for backend url? -> JS dict in launch call (from index.html?)~~
  - ~~prod? (oob from native package, possibly overridable but v.low prio)~~
- ~~copy frontend output to backend (-app module?)~~
- ~~dev-server~~
  - ~~scaffolding (index.html, launcher, flags)~~
  - ~~sbt tasks~~

### backend
- ~~native-packager~~

### data-tools

### CI
- GH action?

## features
- ~~linear programming solver~~
- ~~fix ficsmas (find the gift extractor)~~
- ~~minimize transports (logistics option, changes the radios into checkboxes)~~
- ~~v-align icons inside text~~
- ~~resource settings in SolverRequest~~
- ~~canCompute on presentation-altering changes in plan options~~
- ~~Edit request amounts & remove requests~~
- ~~HotReload would be nice starting about right now. Or just store in LocalStorage ourselves?~~
- Extract "generic" HotReloadApp

### data-tools
- ~~load GameData~~
- ~~load Model & write model **to JSON**~~
- ~~Why are some recipes with Ionized fuel in T8?~~
- move SolutionSampleExporter to a tool, and code-gen the solutions.json
- evaluate "potential for amplification" on recipes statically

### backend
- ~~import resources~~
- ~~provide model(s) - from JSON rather than db?~~
- ~~serve static assets~~
- ~~serve index.html for prod~~
- ~~fix: solver's item weight should be inversely proportional to cap~~
- ~~durable storage of plans~~
- ~~persistence: store model version of plans!~~
- ~~persistence: store (optional) plan solution~~
- ~~persistence: filesystem storage module?~~
- persistence: replace Int ids with UUIDs (for concurrent mod)
  - NOTE update doMigration/migratePlan to allow file name changes
    - do the write to a temp dir, remove the original and move the new
    - have PPA.writePlanFiles return the file names

## frontend
- ~~"deep" internal navigation~~
- ~~Extraction recipes in table~~
- Plan Header: use dropdowns to group buttons? 
- Recipe options: disable alt/matconv toggles if no effect?
- Recipe options: align the two rows of tier buttons?
- Resource options: button to reset resource nodes to full amount
- ~~PlanTable: totals (machines at least, maybe resources)~~
- ~~"My Plans" + save/load~~
- ~~Filter request selection by what is feasible~~
- ~~Maybe stop it with the hero in the plan header~~
- ~~Rework request selection (use top space better, remove hero, extract requested out of summary)~~
- ~~"My Plans": sort the table~~
- Dom.focus not working, try tricks (like including the target in the DOM with display: hidden)?
- utility to replace all calls to `List[Attr[Nothing]]( ... )` to force conversions
- ~~duplicate plan~~
- compare plans (compare with saved?)
- plan table: buttons in header to expand/collapse all rows
- integrate js LP lib to have a frontend Solver, with potential implementations:
  - https://www.npmjs.com/package/javascript-lp-solver
  - https://github.com/IanManske/YALPS
  - https://www.npmjs.com/package/glpk.js
- ~~cache-busting~~
- ~~recipe row: estimate building footprint~~
  - with options for clearance/etc.
  - simple footprint per (leaf?) group (Dijsktra)
- ~~Organizer: affordance for splitting transports (and not just processes)~~
- Group visibility toggle (both views or only plan?)
- Affordance for drilling down into peer groups in gorup transports
- ~~Power amplification~~
- Change logic of extraction recipes when overclocking (use all, then overclock)
- hover with more precision for (some) doubles
- favorite recipes (set in browse?)
- Flows: show the amounts on each side when unbalanced
- Flows: invalidate Transport splits only when they become invalid after move/merge

#### fixes
- ~~Requests panel looks like ass~~
- ~~"Add request" button no worky in organize view~~
  - ~~disable it in that view~~
- ~~Something is wrong with resource caps? (water, FICSMAS gift)~~
- Something is also wrong with "hide FICSMAS" in plan
- ~~new request panel: restore the \<input>s~~
- ~~revert is weird on local reload~~ OK now?
- ~~Group summary import/exports need to take into account whether the group is a net importer, exporter or neither~~
- Group grid: incorrect fill or presence of grand-nibling groups (relative to current)
- Group summary
  - flat/transport toggle incorrectly shown when there are no group transports
  - still not exactly right re: transport (count), fiddle with net imports/exports
- maybe using Tag.withKey could improve vdom patching perf (target all the groupGrid 1st)
- ~~need to update REQUESTED in flows when recipes do not change :/~~
- Library: deleting a plan should reload the library or apply the deletion to the model

#### the quest for flows

- ~~action button state (availability) - moves only~~
- ~~state conversion to persistent~~
- ~~local state persistence (hot reload)~~
- ~~action implementation~~
  - ~~start with moves~~
  - ~~split~~
  - ~~merge~~
  - ~~other actions, tweaks~~
    - ~~mod (shift) for "bump" (move to new ItemTransport before/after)~~
    - ~~add equal split with arg. (or change equal split w/ default)~~
    - ~~split remaining capacity (when other end has less total amount but enough for some of this flow)~~
- ~~organized plan in PlanTable~~
- ~~persistence~~
- ~~groups~~
- ~~group summary~~
- ~~replace notification blocks with message?~~
- ~~show group in flows view~~
- ~~group grid~~
  - ~~show current group~~
  - ~~group swap - or maybe on group summary's group button~~ 
- undo stack
- ~~process expanded rows: show group(s) in I/O~~
  - ~~also refactor away from ItemIO?~~

## refactorings vs. legacy
- ~~check circe codecs in tools for actual use~~
