# PR 133 B4 和 B5 延期交接

## 停止点

本轮工作按用户要求停在 B3 完成之后。B1、B2、B3 的修改和对应验收已经完成；B4 与 B5 尚未开始。本文件只记录后续工作，不实施 B4 或 B5，也不授权推送、变基、历史改写或主 checkout 清理。

冻结坐标如下：

| 坐标 | 值 |
| --- | --- |
| 官方 PR base | `96d7089b1935a6ec4d1e359329523a5fe8982791` |
| 原 PR head | `21abba2ee34a5882c470bb740d892a2ed2b8f781` |
| 当前 clean endpoint | 在当前 worktree 执行 `git rev-parse HEAD` 获取 |
| clean handoff 分支 | `relay/pr133-clean-handoff-20260915` |
| clean handoff worktree | `D:\UnderGraduateFiles\Research\Ant\open64\open64-pr-133-clean` |
| 本地私有恢复引用 | `relay/pr133-fixes-audit-20260915` |
| 远端状态 | 尚未 push；本地分支未设置 upstream |

`relay/pr133-fixes-audit-20260915` 只作为本地私有恢复引用：不交给合作者、不 push，也不作为后续开发基线。对外 handoff 只使用 clean 分支。可用 `git log --reverse --oneline 21abba2e..HEAD` 解析 clean history，用 `git rev-parse HEAD` 解析当前 endpoint；本文不自引用尚未知的提交 hash。

## B1 至 B3 已完成结果

### B1 SYNC 3 权威和验证状态

B1 已完成文档权威、历史结果、当前验证状态、跨平台 policy digest 和 fail-closed handoff gate 的修复。B1 修复本身已经通过相应静态和 Python 测试，但这不等于重新取得了当前 v0.10 独立认证。

### B2 v0.10 安全边界

B2 已恢复 v0.10 的 client-owned secret key、server evaluation-only material、ciphertext transport 和 server-without-secret-key 完成门槛。ACE `FHErt_ant` 仍是团队批准的早期 provider，但该决定只覆盖 provider/API，不覆盖或降低 v0.10 的安全语义。

B2 同时冻结了 capability producer/consumer 顺序：

```text
SYNC-4 ReLU/bootstrap materialized subset
  -> SYNC-5 complete full-model schedule and capability manifest
  -> SYNC-6 ACE consumer and acceptance
```

### B3 唯一 ABI 合同

`doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md` 已成为 SYNC-5 和 SYNC-6 唯一的公开 ABI v1 合同。它定义了完整 ResNet call census、语义 schedule、model package、broker、worker、handle/value identity、capability、import/export、ownership、alias、failure、retry 和 artifact sealing 规则。

B3 文档合同审查结果为：

```text
P0 = 0
P1 = 0
P2 = 0
Contract tests = 48/48 passed
```

该结论只覆盖文档合同及其合同测试。下列真实实现尚不存在或尚未执行验证：

- 真实公开 C header；
- mock runtime；
- broker；
- supervised worker；
- ACE provider adapter；
- `FHErt_ant` 运行时集成；
- 完整 generated-C compile/link/runtime 路径。

exact ACE pin 尚未实证能够 import evaluation-only public context、evaluation/relinearization/rotation/bootstrap keys 和 ciphertext，同时不创建、不持有 secret key。因此 SYNC-6 必须保持 fail-closed。若 exact pin 不能满足该边界，需要单独审查的 ACE patch 或新的 immutable pin；不能以 embedded key lifecycle 替代 v0.10 gate。

clean handoff history 将 B1、B2、B3 各收敛为一个逻辑提交。本文件随第三个 B3 提交交付，仅记录停在 B3 时仍需完成的 B4/B5；它不代表 B4/B5 已开始。

## SYNC 3 重裁矩阵

SYNC-3 必须同时表达历史事实、当前证据债务和严格消费条件。不得把历史 Pass、owner 接受风险或设计准备资格改写为当前独立认证已完成。

| 维度 | 状态 | 含义 |
| --- | --- | --- |
| Implementation | `MERGED` / `PASS` | PR #131 的实现已经合入；实现审查有历史 Pass 记录 |
| Historical certification | `PASS_RECORDED` | 2026-09-14 merged-tip certification 被记录为 Pass，但本轮未重新执行 |
| Evidence debt | `OWNER_ACCEPTED` | owner 可以承认现有证据债务并决定是否承担继续设计的风险；这不是验证结果 |
| Current v0.10 independent certification | `UNVERIFIED` | 当前 reviewer 无法访问完整 retained bundle bytes，无法独立重开并复核 |
| Master-plan completion | `NOT_ESTABLISHED` | v0.10 Architecture Phase 3、M4 或更高层 complete 状态未由本轮建立 |
| Design preparation | `ALLOWED` | 可以继续合同、文件 ownership、测试矩阵和实现方案设计 |
| Strict certified-input consumption | `false` | 未取得当前独立认证时，不得把 SYNC-3 当作严格 certified input 消费 |

建议保留以下机器可读状态：

```text
SYNC3_IMPLEMENTATION_STATE=MERGED
SYNC3_IMPLEMENTATION_VERDICT=PASS
SYNC3_HISTORICAL_CERTIFICATION=PASS_RECORDED
SYNC3_EVIDENCE_DEBT=OWNER_ACCEPTED
SYNC3_CURRENT_V010_INDEPENDENT_CERTIFICATION=UNVERIFIED
SYNC3_MASTER_PLAN_COMPLETION=NOT_ESTABLISHED
SYNC4_DESIGN_PREPARATION_ALLOWED=true
SYNC4_STRICT_CERTIFIED_INPUT_CONSUMPTION=false
```

### Owner evidence waiver

截至当前 clean endpoint，owner evidence waiver **尚未签发**。如果 owner 正式决定在证据债务未清偿时允许 SYNC-4 implementation 消费历史证据，必须另行记录 waiver，至少写明 owner、日期、作用范围、已知缺口、风险承担、过期或撤销条件以及后续补证门槛，并使用：

```text
SYNC4_CONSUMPTION_BASIS=OWNER_EVIDENCE_WAIVER
```

采用 waiver 时，下列状态仍必须保持：

```text
SYNC3_CURRENT_V010_INDEPENDENT_CERTIFICATION=UNVERIFIED
```

不得把 waiver、owner acceptance、历史 hash 列表或历史 Pass 写成 `VERIFIED`，也不得据此宣称 master-plan completion 已建立。

### Retained bundle blocker

历史 certification 指向当前 reviewer 无法访问的 `/private/tmp/...`。hash 列表只能标识候选 bytes，不能替代 bytes 本身。严格重认证至少需要一个可访问、不可变、内容寻址的完整 bundle，包含：

- exact source commit 和 tree；
- source fixture、checkpoint、dataset/selection 和 toolchain identity；
- 原始、转换后 `.B` 与 `ir_b2a -st -src` `.T`；
- 原始和转换后 payload；
- conversion、shape、CKKS state、disposition 和 accuracy reports；
- positive、negative、rollback、no-partial-output 和 compatibility logs；
- 所有执行命令和环境信息；
- bundle manifest 与逐文件 SHA-256；
- 独立 reviewer 的 reopen、hash、语义和失败路径结论。

在该 blocker 清除前，严格 certified-input consumption 必须为 false。

## B4 待办

B4 尚未执行。它只能修改：

```text
doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
```

建议 commit claim：

```text
docs: align the SYNC-4 through SYNC-6 handoff
```

### 输入

- 当前 clean B3 endpoint（在 clean worktree 执行 `git rev-parse HEAD` 获取）；
- `doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx`；
- `doc/FHE-CONSOLIDATED-IMPLEMENTATION-PLAN.md`；
- `doc/FHE-WHIRL-INTEGRATION-PLAN.md`；
- `doc/FHE-DSL-INTEGRATION-PLAN.md`；
- `doc/FHE-ACE-RTLIB-RUNTIME-DECISION.md`；
- `doc/FHE-RUNTIME-C-ABI-V1-CONTRACT.md`；
- owner 对 strict recertification 或 formal waiver 的明确选择。

### 必须完成的修改

1. Authority Set 首项必须是 v0.10 DOCX，并记录固定 SHA-256 与当前 blob ID 的获取方法。
2. Starting Boundary 必须采用 SYNC-3 双轨状态：historical `PASS_RECORDED` 与 current `UNVERIFIED` 同时存在。
3. 若 owner 采用 waiver，handoff 必须记录 `SYNC4_CONSUMPTION_BASIS=OWNER_EVIDENCE_WAIVER`，但 verification 仍写 `UNVERIFIED`。
4. SYNC-4 capability gate 仅覆盖已经物化的 ReLU/bootstrap subset，包括 target levels、slots、depth 和 `7 -> 15 -> 13` 所需能力。
5. SYNC-5 必须冻结完整 ResNet ABI/mock/call census、semantic schedule、operation descriptors、full-model capability/key manifest 和 retained artifacts。
6. 完整 rotation/key set 只能由 SYNC-5 已物化 full-model schedule 产生，并在 SYNC-6 消费前通过验证。
7. SYNC-6 必须描述 client/server trust-domain separation、broker、supervised worker、evaluation-only import、ciphertext import/export、no-secret server 和 child termination/status translation。
8. 删除或降级所有把 embedded/local key generation、local decrypt 或 server-without-secret-key 推迟到 post-SYNC6 的旧表述。embedded harness 只能是 non-gating diagnostic lane。
9. Required docs、artifact families、文件 ownership、reviewer separation、stop rules 和 fail-closed 条件必须与 B1 至 B3 一致。
10. handoff 不得创建第二套 ABI、第二套 schedule schema 或 provider-private generated-C surface。

### 输出

- 一个只修改 handoff 文件的 B4 commit；
- 可机器检查的 authority、SYNC-3 basis、S4/S5/S6 producer-consumer、security 和 stop 条件；
- 不新增产品实现，不宣称 runtime 已通过。

### 建议验收命令

```powershell
git diff --check <clean-b3-head> HEAD
git diff --name-only <clean-b3-head> HEAD
git rev-parse HEAD:doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx
Get-FileHash -Algorithm SHA256 doc/DSC_FHE_Compiler_Architecture_and_Integration_Plan_v0.10.docx
python -m unittest osprey.torch2whirl.python.tests.test_fhe_sync3_plan_consistency
python -m unittest osprey.torch2whirl.python.tests.test_fhe_runtime_abi_contract
rg -n "SYNC3_CURRENT|SYNC4_CONSUMPTION_BASIS|server-without-secret-key|evaluation-only|semantic schedule|capability manifest|rotation" doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md
```

`git diff --name-only` 必须只输出 `doc/FHE-SYNC4-TO-SYNC6-TEAM-HANDOFF.md`。还要解析 handoff 中全部 repository-relative `doc/...` 引用，并确认每个路径存在。

### 回滚依赖

B4 依赖 B1、B2、B3。若 B1 至 B3 中任一语义被撤回，必须先 revert B4。B4 本身应保持单文件、单提交，可单独 `git revert`，不得用 reset 或改写审计分支。

## B5 待办

B5 尚未执行。B5 是只读 plan-driven commit review 和 endpoint certification；PR 内不应因 B5 产生产品或计划修改。中文报告更新写入仓库外的 `review-output/pr133`。

### 审查范围

同时审查：

```text
21abba2ee34a5882c470bb740d892a2ed2b8f781..<future-head>
96d7089b1935a6ec4d1e359329523a5fe8982791..<future-head>
```

第一段验证每个修复提交自己的 claim；第二段验证相对官方 PR base 的最终组合 endpoint。必须枚举所有 commit、识别 merge、记录 excluded/included 语义，并保证 commit-index 行数等于实际 commit 数。

### 11 项 finding closure matrix

B5 必须逐项给出 `closed`、`live`、`repaired`、`blocked` 或 `unverified`，并附 commit、文件、测试和 endpoint 证据：

```text
S3-1
S3-2
S3-3
PLAN-1
PLAN-2
PLAN-3
PLAN-4
ACE-1
ACE-2
ACE-3
ACE-4
```

S3-2 在没有完整 retained bundle bytes 时只能是 `blocked` 或 `unverified`，不能因为 owner waiver 改成 `closed`。

### 必须验证的内容

- v0.10 DOCX SHA-256 和 authority order；
- 全部 repository-relative 文档路径存在；
- Markdown fence、table、heading 和内部术语一致；
- `git diff --check`、新增 tab、非预期 binary/path；
- policy JSON canonical LF、exact-byte digest 和 fresh checkout 行为；
- B1/SYNC-3 plan-consistency tests；
- B3 ABI contract tests；
- handoff 的 S4 subset、S5 producer、S6 consumer 顺序；
- security boundary、import/export、worker termination、ownership/alias 和 artifact atomicity；
- 当前 runtime 实现缺失必须明确写成 `SKIP`、`not implemented` 或 `blocked`，不得把 contract-only test 写成 runtime pass；
- Linux fresh checkout 中复跑，不用 Windows 当前 materialized bytes 替代跨平台证明；
- required tool/dependency 不可用时按缺证或 blocked 处理，不得 soft-skip 后报告通过；
- 主 checkout 的用户和并发修改在审查前后完全不变；
- `review-output/pr133` 的中文 README、findings、verification、commit index 和 final assessment 更新到 future endpoint。

### 建议验收命令

```text
git rev-parse 21abba2ee34a5882c470bb740d892a2ed2b8f781 <future-head>
git log --reverse --topo-order --format='%H%x09%P%x09%s' 21abba2ee34a5882c470bb740d892a2ed2b8f781..<future-head>
git rev-list --count 21abba2ee34a5882c470bb740d892a2ed2b8f781..<future-head>
git diff --check 21abba2ee34a5882c470bb740d892a2ed2b8f781 <future-head>
git diff --check 96d7089b1935a6ec4d1e359329523a5fe8982791 <future-head>
python -m unittest osprey.torch2whirl.python.tests.test_fhe_relu_policy_manifests
python -m unittest osprey.torch2whirl.python.tests.test_fhe_sync3_plan_consistency
python -m unittest osprey.torch2whirl.python.tests.test_fhe_runtime_abi_contract
git status --short --branch
```

需要 runtime、native compiler 或 exact ACE 的命令必须在对应 Linux/Docker/fresh-checkout 环境单列。未执行的命令必须进入 limitations，不得从文档测试推断真实 header、mock、broker、worker 或 ACE runtime 已工作。

## Clean handoff history

当前 clean 分支从原 PR head `21abba2ee34a5882c470bb740d892a2ed2b8f781` 开始，应恰好包含三个已完成逻辑批次：

```text
docs/tests: reconcile and harden SYNC-3 certification
docs: restore v0.10 security boundaries and stage gates
docs/tests: define the SYNC-5 ABI and SYNC-6 ACE contract
```

使用以下命令解析实际 hash 和顺序，不在本文硬编码第三个提交的自引用 hash：

```text
git log --reverse --format="%H %s" 21abba2e..HEAD
git rev-parse HEAD
```

B4 完成后才新增第四个逻辑提交：

```text
docs: align the SYNC-4 through SYNC-6 handoff
```

B5 是随后进行的只读终审，不应为 PR 制造产品或计划提交。不得将本文件的存在解释为 B4 或 B5 已完成。

## 恢复顺序

后续恢复工作时按以下顺序进行：

1. 只读核验 clean worktree 的 HEAD、branch、status 以及三个逻辑提交；同时确认本地私有 audit 恢复引用未移动。
2. 用户或 owner 明确选择严格 SYNC-3 re-certification，或正式记录 evidence waiver。没有明确 basis 时不得开始 B4。
3. 在 clean 分支实施单文件 B4，并完成 B4 验收。
4. 冻结 future HEAD，执行完整 B5 commit-by-commit 与 endpoint 审查。
5. 更新仓库外中文 review reports，给出 go/no-go 和所有未执行验证。
6. 对外只交 clean 分支；本地私有 audit 恢复引用不得 push。

## 完成门槛

- B4 仅修改 handoff 文件且通过引用、hash、diff 和合同测试；
- SYNC-3 consumption basis 明确，waiver 不伪装成 verification；
- B5 覆盖两个范围和全部 commit；
- 11 项 finding 均有 endpoint 状态和证据；
- v0.10 security boundary 在所有 active documents 中一致；
- ABI v1 只有一个规范来源；
- runtime 未实现和未测试部分被明确隔离；
- exact ACE evaluation-only import/export 仍未实证时，SYNC-6 保持 blocked；
- PR worktree clean；
- 主 checkout 前后状态完全未被本任务改变；
- 用户明确选择并授权最终推送路径。

## Known blockers

1. 当前 reviewer 无法访问历史 SYNC-3 retained bundle bytes，current v0.10 independent certification 为 `UNVERIFIED`。
2. strict SYNC-3 re-certification 与 formal owner evidence waiver 尚未选择。
3. exact ACE pin 的 evaluation-only import/export 与 no-secret server 能力未实证。
4. 真实 header、mock、broker、worker、ACE adapter 和 runtime path 未实现或未测试。
5. B4、B5 按本轮停止点仍为 deferred；当前 clean handoff 不是 merge-ready 结论。
6. 完整 Linux/fresh-checkout/native/ACE 测试环境可用性尚未重新确认。
7. 主 checkout 存在用户或并发工作，任何写操作都有覆盖风险。

## 主 checkout 保护

本文件创建前的只读状态快照显示，主 checkout 位于 `codex/fhe-o2-integration-plan`，并存在与 PR #133 无关的已修改和未跟踪内容。这些内容没有复制到 clean worktree，也不属于本 handoff；其作者、完成度和用途不得由 PR #133 工作推断。快照在审查期间曾发生变化，说明主 checkout 正被用户或并发任务使用，任何后续工作都必须重新读取实际状态，不能把旧快照当作锁定清单。

后续恢复时必须重新执行只读 `git status --short --branch`，因为上述状态可能继续变化。不得 checkout、restore、add、stash、clean、reset、move、delete、格式化或提交主 checkout 中任何内容。

## Push 边界

clean 分支可以作为明确标注“停在 B3、B4/B5 deferred”的 handoff 分支推送到个人 fork；这种推送不等于批准合并。满足以下任一条件时禁止 push：

- 用户尚未明确授权 push；
- 推送目标分支或 PR source ref 未确认；
- 推送来源不是 `relay/pr133-clean-handoff-20260915`；
- clean history 不是从原 PR head 开始的三个已验收逻辑提交；
- 推送或交接说明把 deferred 的 B4/B5 写成已完成；
- SYNC-3 consumption basis 未明确；
- 任何文档把 waiver 或历史 Pass 错写为 current `VERIFIED`；
- B1 至 B3 endpoint 仍有未解释的 P0、P1 或 P2 finding；
- PR worktree 不 clean 或出现非本任务文件；
- push 需要 force，或目标不是用户个人 fork 上的新分支；
- 操作可能触碰主 checkout 的用户/并发修改。

不得 push `relay/pr133-fixes-audit-20260915`。即使 clean handoff 达到可推送状态，也不得把合同测试的 48/48 描述为真实 runtime certification，且不得把尚未实证的 ACE capability 写成已经通过。B4/B5 完成并通过终审之前，不得把该分支描述为 merge-ready。
