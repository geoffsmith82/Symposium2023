## Embeddings

### Setting up PostgreSQL and PGVector on Ubuntu 22.04

After doing a default install of Ubuntu 22.04

#### Install PostgreSQL

```
sudo apt install postgresql postgresql-server-dev-14
```

#### Compile and Install PGVector extension

```
cd /tmp
git clone --branch v0.4.1 https://github.com/pgvector/pgvector.git
cd pgvector
make
make install # may need sudo
```

#### Setup PostgreSQL to allow remote connections

postgresql.conf
pg_hba.conf

#### Create and Configure database to store Embeddings

Start psql
```
CREATE DATABASE Embeddings;
```

```
CREATE USER EmbedUser WITH ENCRYPTED PASSWORD 'SomePassword';
GRANT ALL PRIVILEGES ON DATABASE embeddings to EmbedUser;
```


#### Create table to store embeddings


TODO:


#### Install PostgreSQL ODBC drivers

TODO:

#### Setup FireDAC connection to connect to DB


TODO: