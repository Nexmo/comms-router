<template>
  <div> 
    <div v-if="rows.length > 0" class="Vlt-table Vlt-table--data">
      <table>
        <thead>
          <tr>
            <th v-for="col in columns" @click="sort(col)" 
              :class="{
                'Vlt-table__col--sortable': col.sortable,
                'Vlt-table__col--ascending': currentSortColumn === col && !isAsc,
                'Vlt-table__col--descending': currentSortColumn === col && isAsc,
              }">
              {{col.title}}
            </th>
          </tr>
        </thead>
        <tbody>
          <tr v-for="(row, index) in visibleRows" @click="clickRow(row)">  
            <slot name="item" :item="row" :index="index"></slot>
          </tr>
        </tbody>
      </table>
    </div>

    <div v-if="rows.length > 0 && pagination" class="Vlt-table__pagination">      
      <ul>
          <li v-for="page in pages" :key="page.number" 
             :class="{ 'Vlt-table__pagination__current': page.number === newCurrentPage }">
              <a href="#" @click.prevent="page.click">{{ page.number }}</a>
          </li>
      </ul>
    </div>

    <div v-if="rows.length === 0" class="Vlt-empty Vlt-empty--search">
      <div class="Vlt-empty__content">
        <p>There are no results for this search.</p>
      </div>
    </div>
  </div>
</template>

<script>
    export default {
        name: 'vlt-table',

        //todo: currently sorting / pagination is only client side, improve 
        //so that if available sorting / pagination is doen by the server 

        props: {
          columns: {
              type: Array,
              default: () => []
          },
          currentPage: {
              type: Number,
              default: 1
          },
          pagination: Boolean,
          pageSize: {
              type: [Number, String],
              default: 20
          },
          rows: {
              type: Array,
              default: () => []
          },
          total: {
              type: [Number, String],
              default: 0
          }
        },

        data() {
          return {
            currentSortColumn: {},
            isAsc: undefined,
            newRows: this.rows,
            newRowsTotal: this.rows.length,
            newCurrentPage: this.currentPage
          }
        },

        computed: {
            visibleRows() {
                if (!this.pagination) return this.newRows;

                const currentPage = this.newCurrentPage;

                if (this.newRows.length <= this.pageSize) {
                    return this.newRows;
                } else {
                    const start = (currentPage - 1) * this.pageSize;
                    const end = parseInt(start, 10) + parseInt(this.pageSize, 10);
                    return this.newRows.slice(start, end);
                }
            },

           pages() {
              const left = Math.max(1, this.currentPage - 1);
              const right = Math.min(this.currentPage + 1, this.newRowsTotal / this.pageSize);

              const pages = [];
              let vm = this;
              for (let i = left; i <= right; i++) {
                  pages.push({
                      number: i,
                      click: (event) => {
                          this.pageChanged(i);
                          // Set focus on element to keep tab order
                          this.$nextTick(() => event.target.focus());
                      }
                  })
              }
              return pages;
          }
        },

        watch: {
            rows(newRows) {
              this.newRows = newRows;             
              this.sort(this.currentSortColumn, true);        
              this.newRowsTotal = newRows.length;
            },

            currentPage(newCurrentPage) {
                this.newCurrentPage = newCurrentPage;
            }
        },

        methods: {
            sortBy(array, property, isAsc) {

                let sorted = [...array].sort((a, b) => {
                    let aVal = a[property];
                    let bVal = b[property];

                    if (!aVal && aVal !== 0) return 1;
                    if (!bVal && bVal !== 0) return -1;
                    if (aVal === bVal) return 0;

                    aVal = (typeof aVal === 'string')
                        ? aVal.toUpperCase()
                        : aVal;

                    bVal = (typeof bVal === 'string')
                        ? bVal.toUpperCase()
                        : bVal;

                    return isAsc
                        ? aVal > bVal ? 1 : -1
                        : aVal > bVal ? -1 : 1;
                });

                return sorted;
            },

            sort(column, updatingData = false) {
                if (!column || !column.sortable) return;

                if (!updatingData) {
                    if(!this.isAsc) this.isAsc = true;
                    else {
                      this.isAsc = column === this.currentSortColumn
                        ? !this.isAsc 
                        : true;
                    }
                    
                }
                this.newRows = this.sortBy(
                    this.newRows,
                    column.property,
                    this.isAsc
                )
                this.currentSortColumn = column;
            },

            clickRow(row) {
                this.$emit('click', row);
            },

            pageChanged(page) {
                this.newCurrentPage = page;
                this.$emit('page-change', this.newCurrentPage);
            }
        }
    }
</script>