# --- !Ups

create table tx (
    tx_date char(8),
    description varchar(255),
    amount double
);
alter table tx add constraint tx_pk unique(tx_date, description, amount);


# --- !Downs

drop table tx;
