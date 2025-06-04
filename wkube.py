from accli import WKubeTask

task = WKubeTask(
    name="Biodiversity Indicators Exp1",
    job_folder='./',
    base_stack='R4_4',
    command="Rscript main.R",
    required_cores=1,
    required_ram=1024*1024*512,
    required_storage_local=1024*1024*2,
    required_storage_workflow=1024*1024,
    timeout=60*60,
    conf={
        "input_mappings": "acc://act4cap27/agmip_data/Cleaned_AGMIP_Data.csv:/code/inputs/input_data.csv",
        "output_mappings": "/code/outputs/:acc://out"
    }
)

# python -m accli login --webcli=https://localhost:8080 -s=http://web_be:8000
# python -m accli dispatch bightspace task -s=http://web_be:8000