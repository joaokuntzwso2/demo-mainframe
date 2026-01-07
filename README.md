# Mainframe Emulator + WSO2 Micro Integrator Demo 

This project is a self-contained demo environment that simulates a “modern API façade over legacy mainframe batch programs”.

It runs three containers:

1. **Mainframe emulator (TK4- / Hercules)**  
   Runs an emulated IBM mainframe environment with JES/job submission via a **socket reader** and output spooled to a **printer file**.

2. **Adapter (Flask)**  
   A lightweight service that:
   - submits JCL to the mainframe emulator over TCP (socket reader port)
   - polls a printer output file for a `JSON:` line produced by the COBOL program
   - returns that JSON as the HTTP response

3. **WSO2 Micro Integrator (MI)**  
   Exposes modern REST endpoints that translate HTTP requests into JCL payloads and calls the adapter.
   MI is the integration “bridge” layer and is the component you will extend in the future to add policies, transformations, orchestration, etc.

---

## High-level Architecture

```

Client (curl / Postman / APIM)
|
v
+-----------------------------+
| WSO2 Micro Integrator (MI)  |
| REST API: /mf/...           |
+-------------+---------------+
|
| HTTP POST /submit
v
+-----------------------------+
| Adapter (Flask)             |
| - sends JCL to socket reader|
| - tails printer file for JSON
+-------------+---------------+
|
| TCP to socket reader
v
+-----------------------------+
| TK4-/Hercules Mainframe     |
| - receives JCL              |
| - runs COBOL batch programs |
| - writes output to printer  |
+-----------------------------+

```

---

## Repository Structure

```

.
├── docker-compose.yml
├── adapter/
│   ├── Dockerfile
│   ├── app.py
│   └── requirements.txt
├── mainframe/
│   ├── cobol/     # COBOL source programs (reference)
│   ├── jcl/       # JCL jobs to load/compile/run programs
│   └── out/       # Printer output files (mounted volume)
└── mi/
├── pom.xml
├── src/main/wso2mi/artifacts/...  # MI Synapse artifacts (API)
└── deployment/deployment.toml

```

---

## What This Demo Does

The demo exposes “legacy banking” operations through modern REST APIs:

### 1) Account Inquiry
- Legacy COBOL program: `ACCTINQ`
- Input: `ACCTID=<id>`
- Output: JSON line printed to SYSOUT (printer) like:
  `JSON:{"type":"AccountInquiry", ...}`

REST:
```

GET /mf/account/{id}

```

### 2) Customer Profile
- COBOL program: `CUSTPROF`
- Input: `CUSTID=<id>`
REST:
```

GET /mf/customer/{id}

```

### 3) Transaction Posting
- COBOL program: `TXNPOST`
- Input lines:
  `FROM=...`
  `TO=...`
  `AMOUNT=...`
  `CCY=...`
REST:
```

POST /mf/txn
{
"from": "...",
"to": "...",
"amount": "...",
"currency": "LKR"
}

````

---

## How It Works (Detailed)

### A) Mainframe Emulator (tk container)
The `tk` container runs a TK4-/Hercules environment configured with:
- TN3270 access on port **3270** (interactive terminal)
- a web console on port **8038**
- a **socket reader** on port **3505** (accepts raw JCL submitted via TCP)

The repository mounts `./mainframe` into the container as `/demo`:

- `/demo/jcl` contains JCL jobs used for setup and running programs
- `/demo/out` is used for printer outputs (SYSOUT spools written to file(s))

### B) Adapter Service (adapter container)
The adapter exposes:

- `GET /health`  
  Shows config: host, port, printer file path.

- `POST /submit`  
  Body:
  ```
  { "jcl": "<JCL TEXT>", "expectJson": true }
````

Flow:

1. Takes JCL as text.
2. Opens a TCP socket to `tk:3505` and sends the JCL payload.
3. Records current printer file size (start position).
4. Polls the printer file for new lines, looking for lines starting with `JSON:`.
5. Returns the last JSON line found.

### C) WSO2 Micro Integrator (mi container)

The MI API (`MainframeDemoAPI.xml`) implements the façade endpoints.

For each REST call:

1. Reads URI params / JSON body.
2. Constructs a JCL string using `payloadFactory` and `fn:concat(...)`.
3. Calls `http://adapter:9000/submit`.
4. Responds with whatever JSON comes back.

This is the integration layer you later expand (validation, mapping, orchestration, calling additional systems, etc).

---

## Prerequisites

### Required

* Docker + Docker Compose
* A TN3270 client (optional but recommended for “mainframe feel”):

  * macOS: `x3270`
  * Windows: `wc3270`
  * Any TN3270 emulator is fine

### Notes for Apple Silicon (M1/M2/M3)

The `tk` image is `linux/amd64`. Docker will run it under emulation.
This is normal, but it can be slower.

---

## Configuration

### docker-compose.yml (key settings)

* TK socket reader:

  * `TK_HOST=tk`
  * `TK_SOCKREADER_PORT=3505`

* Printer file (adapter polling target):

  * `PRT_FILE=/demo/out/prt00e.txt`

If your mainframe container produces a different printer file name,
update `PRT_FILE` to match the real output file.

---

## Setup & Run (Step-by-step)

### 1) Clone and enter the repo

```bash
git clone <your-repo-url>
cd Mainframe
```

### 2) Build and start all services

```bash
COMPOSE_BAKE=false docker compose up -d --build
docker compose ps
```

### 3) Check adapter health

```bash
curl -s http://localhost:9000/health | jq .
```

Expected:

```json
{
  "status": "ok",
  "tkHost": "tk",
  "sockReaderPort": 3505,
  "printerFile": "/demo/out/prt00e.txt"
}
```

### 4) Confirm TK outputs are being written (IMPORTANT)

Find printer files inside the `tk` container:

```bash
docker exec -it tk sh -lc 'ls -lah /demo/out; find /demo -maxdepth 4 -type f -iname "prt*" | head -n 20'
```

If you do NOT see `/demo/out/prt00e.txt`, update `PRT_FILE` in `docker-compose.yml`
to the correct path and restart:

```bash
docker compose down
COMPOSE_BAKE=false docker compose up -d --build
```

---

## Mainframe Preparation (Load & Compile COBOL)

The MI APIs assume these load modules exist:

* `IBMUSER.LOAD(ACCTINQ)`
* `IBMUSER.LOAD(CUSTPROF)`
* `IBMUSER.LOAD(TXNPOST)`

To achieve that, you typically perform:

1. Load source/JCL members into PDS
2. Compile & link-edit COBOL programs
3. Run programs

### 5) Submit the setup JCL (create PDS + load members)

Submit `mainframe/jcl/00_LOADSRC.jcl` using adapter:

```bash
curl -s http://localhost:9000/submit \
  -H 'Content-Type: application/json' \
  -d "$(jq -n --arg jcl "$(cat mainframe/jcl/00_LOADSRC.jcl)" '{jcl:$jcl, expectJson:false}')"
```

This job creates datasets (PDS) like `IBMUSER.COBOL` and `IBMUSER.JCL`
and loads COBOL/JCL members via `IEBUPDTE`.

### 6) Compile & link-edit programs

If you have separate compile JCL files (recommended), submit them now:

* `10_ACCTINQ_CMP.jcl`
* `11_CUSTPROF_CMP.jcl`
* `12_TXNPOST_CMP.jcl`

Example:

```bash
curl -s http://localhost:9000/submit \
  -H 'Content-Type: application/json' \
  -d "$(jq -n --arg jcl "$(cat mainframe/jcl/10_ACCTINQ_CMP.jcl)" '{jcl:$jcl, expectJson:false}')"
```

Repeat for the other compile jobs.

> If compile jobs are embedded as members inside 00_LOADSRC, you can alternatively submit those member JCLs from within the mainframe using the 3270 terminal.

### 7) Validate compile results

Look at printer output:

```bash
tail -n 200 mainframe/out/prt00e.txt
```

You want return codes indicating success (often `CC 0000`).

---

## Demo Script (End-to-end)

### A) (Optional) Open the Mainframe Terminal

Use a TN3270 client and connect to:

* Host: `localhost`
* Port: `3270`

This gives the “mainframe operator” experience.

Web console (optional):

* [http://localhost:8038](http://localhost:8038)

### B) Test the adapter directly (bypassing MI)

This proves socket reader + printer parsing works.

#### Account Inquiry

```bash
curl -s http://localhost:9000/submit \
  -H 'Content-Type: application/json' \
  -d '{
    "jcl": "//ACCTRUN JOB (RUN),'\''WSO2DEMO'\'',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)\n//STEP1   EXEC PGM=ACCTINQ\n//STEPLIB DD  DSN=IBMUSER.LOAD,DISP=SHR\n//SYSIN   DD  *\nACCTID=00000001\n/*\n",
    "expectJson": true
  }' | jq .
```

### C) Test via WSO2 Micro Integrator API

These are the “real demo endpoints”.

#### 1) Account Inquiry

```bash
curl -s "http://localhost:8290/mf/account/00000001" | jq .
curl -s "http://localhost:8290/mf/account/00000002" | jq .
curl -s "http://localhost:8290/mf/account/99999999" | jq .
```

#### 2) Customer Profile

```bash
curl -s "http://localhost:8290/mf/customer/C000000001" | jq .
curl -s "http://localhost:8290/mf/customer/C000000002" | jq .
```

#### 3) Transaction Post

```bash
curl -s "http://localhost:8290/mf/txn" \
  -H 'Content-Type: application/json' \
  -d '{"from":"00000001","to":"00000002","amount":"125.50","currency":"LKR"}' | jq .
```

---

## Troubleshooting

### Adapter keeps restarting

Cause: Flask app is not started (missing `app.run(...)`) or CMD is wrong.
Fix: Ensure `adapter/app.py` includes:

```python
if __name__ == "__main__":
    app.run(host="0.0.0.0", port=9000)
```

### Adapter returns TIMEOUT_WAITING_FOR_MAINFRAME_OUTPUT

Cause: adapter is polling the wrong printer file OR the job didn’t run.
Steps:

1. Find real printer file:

   ```bash
   docker exec -it tk sh -lc 'find /demo -maxdepth 5 -type f -iname "prt*" | head -n 30'
   ```
2. Update `PRT_FILE` to that exact path.
3. Confirm the job actually ran (check `tk` logs and printer content).

### MI returns 500 but adapter works

Cause: MI → adapter networking/endpoint misconfig.
Validate from inside MI container:

```bash
docker exec -it mi sh -lc 'wget -qO- http://adapter:9000/health || curl -s http://adapter:9000/health'
```

### Platform warning (amd64 vs arm64)

This is expected on Apple Silicon.
If you want to make it explicit, set:

```yaml
platform: linux/amd64
```

for services using amd64-only images.

---

## Stop / Cleanup

Stop services:

```bash
docker compose down
```

Full cleanup including volumes (careful: removes state):

```bash
docker compose down -v
```

---

## Next Steps (Roadmap)

* Add request correlation to adapter (RID) to safely handle concurrent requests.
* Publish the MI API via WSO2 API Manager and apply:

  * rate limiting
  * authentication
  * auditing/analytics
* Expand COBOL logic to simulate additional systems (cards, loans, fraud checks).

