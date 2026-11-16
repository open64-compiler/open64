"""Separate fixed H7 diagnostics using Q7's counter meanings and CSV audit.

No benchmark samples, search feedback or automatic model updates. Missing
metrics/permissions stay unknown. See the H7 qualification specification.
"""
from pathlib import Path
import shutil
from h20_h7_common import write, sha
from mla_q7_profile import COUNTERS, available_metrics, summarize_ncu


def collect(args, out, table, child, step):
    """Profile one representative fixed case per family after qualification/timing."""
    out.mkdir();report=dict(schema='open64.h20-h7.profiling.v1',status='INCOMPLETE',separate_process=True,
        benchmark_samples_emitted=False,search_invocations=0,cases=[],attribution='Causal data-wait and local/peer/remote waiting remain unknown.')
    ncu=shutil.which('ncu');supported=[]
    if ncu and step('h7-ncu-query',[ncu,'--query-metrics','--query-metrics-mode','all','--devices','0'])==0:
        supported=available_metrics((out.parent/'logs/h7-ncu-query.stdout').read_text())
    report['requested_metrics']=supported
    report['unavailable_metrics']=[name for name,unit in COUNTERS.values() if name not in supported]
    for role,cases in table['profiles'].items():
        case=cases[-1];folder=out/case['id'];folder.mkdir();row=dict(case_id=case['id'],status='UNAVAILABLE')
        base=child('profile',folder/'execution')+['--case',case['id']]
        if ncu and supported:
            command=[ncu,'--profile-from-start','off','--target-processes','all','--replay-mode','kernel','--cache-control','all','--clock-control','none',
                '--metrics',','.join(supported),'--csv','--page','raw','--print-units','base','--print-fp','--print-metric-name','name',
                '--print-kernel-base','function','--log-file',folder/'metrics.csv','--export',folder/'counters',*base]
            row['exit_code']=step('h7-profile-'+case['id'],command)
            if row['exit_code']==0:
                try:
                    summary=summarize_ncu((folder/'metrics.csv').read_text())
                    if len(summary['kernels'])!=len(case['kernels']):raise ValueError('counter launch census differs from selected operation')
                    row.update(status='COLLECTED',summary=summary,csv=case['id']+'/metrics.csv',csv_sha256=sha(folder/'metrics.csv'))
                except (OSError,ValueError) as error:row['reason']=str(error)
        if args.profile_timeline:
            nsys=shutil.which('nsys')
            row['timeline_status']='UNAVAILABLE'
            if nsys:
                command=[nsys,'profile','--trace','cuda,nvtx','--sample','none','--cpuctxsw','none','--capture-range','cudaProfilerApi',
                    '--capture-range-end','stop','--output',folder/'timeline',*child('profile',folder/'timeline-execution'),'--case',case['id']]
                row['timeline_exit_code']=step('h7-timeline-'+case['id'],command)
                row['timeline_status']='CAPTURED' if row['timeline_exit_code']==0 else 'FAILED'
        report['cases'].append(row);write(out/'report.json',report)
    if all(x['status']=='COLLECTED' for x in report['cases']):report['status']='COLLECTED'
    write(out/'report.json',report);return report
