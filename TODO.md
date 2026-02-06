# TODO

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
- Why are some recipes with Ionized fuel in T8?

### backend
- ~~import resources~~
- ~~provide model(s) - from JSON rather than db?~~
- ~~serve static assets~~
- ~~serve index.html for prod~~
- ~~fix: solver's item weight should be inversely proportional to cap~~
- ~~durable storage of plans~~
- persistence: store model version of plans!

## frontend
- ~~"deep" internal navigation~~
- ~~Extraction recipes in table~~
- Recipe options: disable alt/matconv toggles if no effect?
- Recipe options: align the two rows of tier buttons?
- Resource options: button to reset resource nodes to full amount
- ~~PlanTable: totals (machines at least, maybe resources)~~
- ~~"My Plans" + save/load~~
- ~~Filter request selection by what is feasible~~
- Maybe stop it with the hero in the plan header
- ~~"My Plans": sort the table~~
- Dom.focus not working, try tricks (like including the target in the DOM with display: hidden)?
- utility to replace all calls to `List[Attr[Nothing]]( ... )` to force conversions

## refactorings vs. legacy
- ~~check circe codecs in tools for actual use~~
